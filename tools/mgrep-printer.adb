-----------------------------------------------------------------------
--  mgrep-printer -- Print mail grep results
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
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body Mgrep.Printer is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   procedure Free is
      new Ada.Unchecked_Deallocation (Mail_Result, Mail_Result_Access);

   procedure Add (Printer : in out Printer_Type;
                  Result  : in out Mail_Result_Access) is
   begin
      if Result = null then
         Put_Line ("Insert null");
      end if;
      Printer.Results.Enqueue (Result);
      Result := null;
   end Add;

   procedure Report (Printer : in out Printer_Type) is
      Result : Mail_Result_Access;
   begin
      loop
         Printer.Results.Dequeue (Result, 0.0);
         if Result = null then
            Put_Line ("Null mail result");
         end if;
         New_Line;
         Put ("From: ");
         Put_Line (To_String (Result.From));
         Put ("To: ");
         Put_Line (To_String (Result.To));
         if Length (Result.Cc) > 0 then
            Put ("Cc: ");
            Put_Line (To_String (Result.Cc));
         end if;
         Put ("Date: ");
         Put_Line (To_String (Result.Date));
         Put ("Subject: ");
         Put_Line (To_String (Result.Subject));
         for Line of Result.Lines loop
            Put ("  ");
            Put_Line (Line);
         end loop;
         Free (Result);
      end loop;

   exception
      when Result_Queue.Timeout =>
         null;
   end Report;

end Mgrep.Printer;
