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
with GNAT.Regpat;
with Mail.Parsers;
with Mgrep.Printer;
package Mgrep.Matcher is

   type Mail_Rule is limited record
      Collector    : Mgrep.Printer.Printer_Access;
      Pattern      : GNAT.Regpat.Pattern_Matcher (1000);
   end record;

   type Mail_Processor (Match : access Mail_Rule) is new Mail.Parsers.Processor with record
      Print_Header : Boolean := True;
      Header_Count : Natural := 0;
      Line_Count   : Natural := 0;
      Match_Found  : Boolean := False;
      Result       : Mgrep.Printer.Mail_Result_Access;
   end record;

   overriding
   procedure New_Mail (Handler : in out Mail_Processor);

   overriding
   procedure Read_Header (Handler : in out Mail_Processor;
                          Name    : in String;
                          Content : in String);

   overriding
   procedure Read_Body (Handler : in out Mail_Processor;
                        Line    : in String);

end Mgrep.Matcher;
