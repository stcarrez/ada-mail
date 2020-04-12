-----------------------------------------------------------------------
--  mail-headers -- Operations on mail headers
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Characters.Handling;
with Unicode.Encodings;
with Util.Strings.Builders;
with Util.Encoders.Quoted_Printable;
package body Mail.Headers is

   use Ada.Characters.Handling;

   function Decode_Quoted (Content : in String;
                           Charset : in String) return String;
   function Decode_Base64 (Content : in String;
                           Charset : in String) return String;

   function Decode_Quoted (Content : in String;
                           Charset : in String) return String is
      Result    : constant String := Util.Encoders.Quoted_Printable.Q_Decode (Content);
      Encoding  : Unicode.Encodings.Unicode_Encoding;
   begin
      if Ada.Strings.Equal_Case_Insensitive (Charset, "UTF-8") then
         return Result;
      end if;
      begin
         if Charset = "646" then
            Encoding := Unicode.Encodings.Get_By_Name ("iso-8859-1");
         else
            Encoding := Unicode.Encodings.Get_By_Name (Charset);
         end if;

      exception
         when others =>
            Encoding := Unicode.Encodings.Get_By_Name ("iso-8859-1");
      end;

      return Unicode.Encodings.Convert (Result, Encoding);
   end Decode_Quoted;

   function Decode_Base64 (Content : in String;
                           Charset : in String) return String is
      Decoder  : constant Util.Encoders.Decoder := Util.Encoders.Create ("base64");
      Result   : constant String := Decoder.Decode (Content);
      Encoding : Unicode.Encodings.Unicode_Encoding;
   begin
      if Ada.Strings.Equal_Case_Insensitive (Charset, "UTF-8") then
         return Result;
      end if;
      begin
         --  646 is an old charset, its successor is iso-8859-1.
         if Charset = "646" then
            Encoding := Unicode.Encodings.Get_By_Name ("iso-8859-1");
         else
            Encoding := Unicode.Encodings.Get_By_Name (Charset);
         end if;

      exception
         when others =>
            Encoding := Unicode.Encodings.Get_By_Name ("iso-8859-1");
      end;
      return Unicode.Encodings.Convert (Result, Encoding);
   end Decode_Base64;

   --  ------------------------------
   --  Decode the mail header value and normalize to an UTF-8 string (RFC2047).
   --  ------------------------------
   function Decode (Content : in String) return String is
      use Util.Strings.Builders;

      C       : Character;
      Pos     : Natural := Content'First;
      Pos2    : Natural;
      Pos3    : Natural;
      Result  : Util.Strings.Builders.Builder (Content'Length + 10);
   begin
      while Pos <= Content'Last loop
         C := Content (Pos);
         if C /= '=' then
            Append (Result, C);
         elsif Pos = Content'Last then
            Append (Result, C);
         elsif Content (Pos + 1) = '?' then
            Pos2 := Util.Strings.Index (Content, '?', Pos + 2);
            if Pos2 > 0 and Pos2 + 3 < Content'Last then
               Pos3 := Util.Strings.Index (Content, '?', Pos2 + 3);
               if Pos3 > 0 and then Pos3 + 1 <= Content'Last and then Content (Pos3 + 1) = '=' then
                  C := Content (Pos2 + 1);
                  case C is
                     when 'q' | 'Q' =>
                        Append (Result, Decode_Quoted (Content (Pos2 + 3 .. Pos3 - 1),
                                                       Content (Pos + 2 .. Pos2 - 1)));

                     when 'b' | 'B' =>
                        Append (Result, Decode_Base64 (Content (Pos2 + 3 .. Pos3 - 1),
                                                       Content (Pos + 2 .. Pos2 - 1)));

                     when others =>
                        null;

                  end case;
                  --  Skip ?=
                  Pos := Pos3 + 1;
                  if Pos < Content'Last and then Is_Space (Content (Pos + 1)) then
                     Pos2 := Pos + 2;
                     while Pos2 <= Content'Last and then Is_Space (Content (Pos2)) loop
                        Pos2 := Pos2 + 1;
                     end loop;

                     --  Skip spaces between consecutive encoded words.
                     if Pos2 + 1 <= Content'Last and then Content (Pos2) = '='
                       and then Content (Pos2 + 1) = '?'
                     then
                        Pos := Pos2 - 1;
                     end if;
                  end if;
               else
                  Append (Result, '=');
               end if;
            else
               Append (Result, '=');
            end if;
         else
            Append (Result, '=');
         end if;
         Pos := Pos + 1;
      end loop;
      return To_Array (Result);
   end Decode;

end Mail.Headers;
