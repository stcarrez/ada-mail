-----------------------------------------------------------------------
--  mail-parsers -- Parse mail content
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
with Ada.IO_Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;
with Util.Strings.Builders;
with Util.Encoders.Quoted_Printable;
package body Mail.Parsers is

   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use Util.Strings.Builders;
   use Util.Encoders.Quoted_Printable;

   HEADER_NAME_LENGTH    : constant := 128;
   HEADER_CONTENT_LENGTH : constant := 512;

   function Is_Space (C : in Character) return Boolean is
      (C = ' ' or C = ASCII.HT);

   procedure Read_Line (Parser : in out Parser_Type) is
      C   : Character;
      Pos : Natural := 0;
   begin
      while not Parser.Reader.Is_Eof and Pos < Parser.Line'Last loop
         Parser.Reader.Read (C);
         exit when C = ASCII.LF;
         if C /= ASCII.CR then
            Pos := Pos + 1;
            Parser.Line (Pos) := C;
         end if;
      end loop;
      Parser.Length := Pos;

   exception
      when Ada.IO_Exceptions.Data_Error =>
         Parser.Length := Pos;
         Parser.Is_Eof := True;
         return;

   end Read_Line;

   procedure Parse_From (Parser : in out Parser_Type) is
   begin
      Parser.Read_Line;
      Parser.State := IN_HEADER;
      Parser.Content_Encoding := Null_Unbounded_String;
   end Parse_From;

   procedure Set_State (Parser : in out Parser_Type;
                        State  : in Parser_State) is
   begin
      if State /= IN_BODY then
         Parser.Content_Encoding := Null_Unbounded_String;
         Parser.State := State;
      elsif Equal_Case_Insensitive (To_String (Parser.Content_Encoding), "quoted-printable") then
         Parser.State := IN_BODY_QUOTED_PRINTABLE;
      elsif Fixed.Index (To_String (Parser.Content_Type), "multipart/") > 0 then
         Parser.State := IN_BODY_PART;
      else
         Parser.State := IN_BODY;
      end if;
   end Set_State;

   procedure Parse_Header (Parser  : in out Parser_Type;
                           Process : access Processor'Class) is
      C       : Character;
      Pos     : Natural;
   begin
      Parser.Read_Line;
      loop
         if Parser.Length = 0 then
            Parser.Set_State (IN_BODY);
            return;
         end if;
         C := Parser.Line (1);
         if not (C in 'A' .. 'Z') then
            Parser.Set_State (IN_BODY);
            return;
         end if;
         Pos := Util.Strings.Index (Parser.Line (1 .. Parser.Length), ':');
         if Pos = 0 then
            Parser.Set_State (IN_BODY);
            return;
         end if;
         declare
            Header  : Util.Strings.Builders.Builder (HEADER_NAME_LENGTH);
            Content : Util.Strings.Builders.Builder (HEADER_CONTENT_LENGTH);
         begin
            Append (Header, Parser.Line (1 .. Pos - 1));
            Pos := Pos + 1;
            while Pos <= Parser.Length and then Is_Space (Parser.Line (Pos)) loop
               Pos := Pos + 1;
            end loop;
            Append (Content, Parser.Line (Pos .. Parser.Length));

            --  Header continues on next line(s).
            loop
               Parser.Read_Line;
               exit when Parser.Length = 0 or else not Is_Space (Parser.Line (1));

               Pos := 2;
               while Pos <= Parser.Length and then Is_Space (Parser.Line (Pos)) loop
                  Pos := Pos + 1;
               end loop;
               Append (Content, Parser.Line (Pos .. Parser.Length));
            end loop;
            declare
               Name : constant String := To_Array (Header);
               Data : constant String := To_Array (Content);
            begin
               if Ada.Strings.Equal_Case_Insensitive (Name, "Content-Type") then
                  Parser.Content_Type := To_Unbounded_String (Data);
               elsif Ada.Strings.Equal_Case_Insensitive (Name, "Content-Transfer-Encoding") then
                  Parser.Content_Encoding := To_Unbounded_String (Data);
               end if;
               Process.Read_Header (Name, Data);
            end;
         end;
      end loop;
   end Parse_Header;

   procedure Parse_Body (Parser  : in out Parser_Type;
                         Process : access Processor'Class) is
      Empty_Line : Boolean := False;
   begin
      while not Parser.Is_Eof loop
         Parser.Read_Line;
         if Empty_Line and then Parser.Length > 10
           and then Parser.Line (1 .. 7) = "From - "
         then
            Parser.State := IN_FROM;
            return;
         end if;
         if Empty_Line then
            Process.Read_Body ("");
         end if;
         if Parser.Length = 0 then
            Empty_Line := True;
         else
            Empty_Line := False;
            if Parser.Length > 2 and then Parser.Line (1 .. 2) = "--" then
               null;
            end if;
            if Parser.State = IN_BODY_QUOTED_PRINTABLE then
               Process.Read_Body (Decode (Parser.Line (1 .. Parser.Length)));
            else
               Process.Read_Body (Parser.Line (1 .. Parser.Length));
            end if;
         end if;
      end loop;
   end Parse_Body;

   function Get_Boundary (Parser : in Parser_Type) return String is
      Content_Type : constant String := To_String (Parser.Content_Type);
      Pos          : Natural := Ada.Strings.Fixed.Index (Content_Type, "boundary=");
      Last         : Natural;
   begin
      if Pos = 0 then
         return "";
      end if;
      Pos := Pos + 9;
      if Pos > Content_Type'Last - 6 then
         return "";
      end if;
      if Content_Type (Pos) = '"' then
         Last := Ada.Strings.Fixed.Index (Content_Type, """", Pos + 1);
         if Last = 0 then
            return "";
         end if;
         return "--" & Content_Type (Pos + 1 .. Last - 1);
      else
         Last := Ada.Strings.Fixed.Index (Content_Type, " ", Pos);
         if Last = 0 then
            Last := Content_Type'Last;
         end if;
         return "--" & Content_Type (Pos .. Last);
      end if;
   end Get_Boundary;

   procedure Parse_Body_Part (Parser  : in out Parser_Type;
                              Process : access Processor'Class) is
      Boundary : constant String := Parser.Get_Boundary;
   begin
      while not Parser.Is_Eof loop
         Parser.Read_Line;
         exit when Parser.Line (1 .. Parser.Length) = Boundary;
      end loop;

      loop
         Parser.Set_State (IN_HEADER);
         while not Parser.Is_Eof loop
            Parser.Parse_Header (Process);
            exit when Parser.State /= IN_HEADER;
         end loop;

         while not Parser.Is_Eof loop
            Parser.Read_Line;
            exit when Parser.Line (1 .. Parser.Length) = Boundary;
            if Parser.Line (1 .. Parser.Length) = Boundary & "--" then
               Parser.Set_State (IN_START);
               return;
            end if;
            if Parser.State = IN_BODY_QUOTED_PRINTABLE then
               Process.Read_Body (Decode (Parser.Line (1 .. Parser.Length)));
            else
               Process.Read_Body (Parser.Line (1 .. Parser.Length));
            end if;
         end loop;

         exit when Parser.Is_Eof;
      end loop;
   end Parse_Body_Part;

   procedure Parse (Parser : in out Parser_Type;
                    Stream : in Util.Streams.Input_Stream_Access;
                    Process : access Processor'Class) is
   begin
      Parser.Reader.Initialize (Stream, 128 * 1024);
      loop
         case Parser.State is
         when IN_START | IN_FROM =>
            Parser.Parse_From;
            Process.New_Mail;

         when IN_HEADER =>
            Parser.Parse_Header (Process);

         when IN_BODY | IN_BODY_QUOTED_PRINTABLE =>
            Parser.Parse_Body (Process);

         when IN_BODY_PART =>
            Parser.Parse_Body_Part (Process);

         end case;
         exit when Parser.Is_Eof;
      end loop;
   end Parse;

end Mail.Parsers;
