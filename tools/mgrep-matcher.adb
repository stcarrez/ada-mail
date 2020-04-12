-----------------------------------------------------------------------
--  mgrep-matcher -- Print mail grep results
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
with Ada.Strings.Unbounded;
with Ada.Strings.Equal_Case_Insensitive;
with Mail.Headers;
package body Mgrep.Matcher is
   
   use Ada.Strings.Unbounded;
   use type Mgrep.Printer.Mail_Result_Access;

   overriding
   procedure New_Mail (Handler : in out Mail_Processor) is
   begin
      if Handler.Result /= null and then Handler.Match_Found Then
         Handler.Match.Collector.Add (Handler.Result);
      end if;
      if Handler.Result = null then
         Handler.Result := new Mgrep.Printer.Mail_Result;
      else
         Handler.Result.Subject := Null_Unbounded_String;
         Handler.Result.Cc := Null_Unbounded_String;
      end if;
      Handler.Match_Found := False;
   end New_Mail;

   overriding
   procedure Read_Header (Handler : in out Mail_Processor;
                          Name    : in String;
                          Content : in String) is
   begin
      Handler.Header_Count := Handler.Header_Count + 1;
      if Handler.Print_Header then
         if Ada.Strings.Equal_Case_Insensitive (Name, "Subject") then
            Handler.Result.Subject := To_Unbounded_String (Mail.Headers.Decode (Content));
         end if;
         if Ada.Strings.Equal_Case_Insensitive (Name, "From") then
            Handler.Result.From := To_Unbounded_String (Mail.Headers.Decode (Content));
         end if;
         if Ada.Strings.Equal_Case_Insensitive (Name, "To") then
            Handler.Result.To := To_Unbounded_String (Mail.Headers.Decode (Content));
         end if;
         if Ada.Strings.Equal_Case_Insensitive (Name, "Cc") then
            Handler.Result.Cc := To_Unbounded_String (Mail.Headers.Decode (Content));
         end if;
         if Ada.Strings.Equal_Case_Insensitive (Name, "Date") then
            Handler.Result.Date := To_Unbounded_String (Mail.Headers.Decode (Content));
         end if;
      end if;
   end Read_Header;

   overriding
   procedure Read_Body (Handler : in out Mail_Processor;
                        Line    : in String) is
   begin
      Handler.Line_Count := Handler.Line_Count + 1;
      if GNAT.Regpat.Match (Handler.Match.Pattern, Line) then
         Handler.Match_Found := True;
         Handler.Result.Lines.Append (Line);
      end if;
   end Read_Body;

end Mgrep.Matcher;
