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
with Ada.Strings.Unbounded;
with Util.Strings.Vectors;
private with Util.Concurrent.Fifos;
package Mgrep.Printer is

   type Mail_Result is record
      From         : Ada.Strings.Unbounded.Unbounded_String;
      Subject      : Ada.Strings.Unbounded.Unbounded_String;
      To           : Ada.Strings.Unbounded.Unbounded_String;
      Cc           : Ada.Strings.Unbounded.Unbounded_String;
      Date         : Ada.Strings.Unbounded.Unbounded_String;
      Lines        : Util.Strings.Vectors.Vector;
   end record;

   type Mail_Result_Access is access all Mail_Result;

   type Printer_Type is tagged limited private;

   type Printer_Access is access all Printer_Type'Class;

   procedure Add (Printer : in out Printer_Type;
                  Result  : in out Mail_Result_Access);

   procedure Report (Printer : in out Printer_Type);

private

   package Result_Queue is
     new Util.Concurrent.Fifos (Element_Type     => Mail_Result_Access,
                                Default_Size     => 1000,
                                Clear_On_Dequeue => True);

   type Printer_Type is tagged limited record
      Results : Result_Queue.Fifo;
   end record;

end Mgrep.Printer;
