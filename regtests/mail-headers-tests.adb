-----------------------------------------------------------------------
--  mail-headers-tests -- Tests for headers
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

with Util.Test_Caller;
package body Mail.Headers.Tests is

   pragma Warnings (Off, "*line is too long*");

   package Caller is new Util.Test_Caller (Test, "headers");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Mail.Headers.Decode",
                       Test_Decode'Access);
   end Add_Tests;

   procedure Test_Decode (T : in out Test) is
   begin
      Util.Tests.Assert_Equals
        (T, "0.9909" & ASCII.CR, Decode ("=?iso-8859-1?Q?0.9909=0D?="),
         "Invalid decode");
      Util.Tests.Assert_Equals
        (T, "définitive de votre compte de courriel NERIM.FR!",
         Decode ("=?iso-8859-1?Q?d=E9finitive_de_votre_compte_de_courriel_NERIM.FR!?="),
         "Invalid decode");
      Util.Tests.Assert_Equals
        (T, "Re: Télé-réunion du bureau",
         Decode ("=?UTF-8?B?UmU6IFTDqWzDqS1yw6l1bmlvbiBkdSBidXJlYXU=?="),
         "Invalid decode");
      Util.Tests.Assert_Equals
        (T, "Re: Télé-réunion du bureau",
         Decode ("Re: =?iso-8859-1?B?VOls6S1y6XVuaW8=?= =?iso-8859-1?Q?n?= du bureau"),
         "Invalid decode");
      Util.Tests.Assert_Equals
        (T, "MySQL <email-mysql@sun.com>",
         Decode ("=?utf-8?B?TXlTUUw=?= <email-mysql@sun.com>"),
         "Invalid decode");
      Util.Tests.Assert_Equals
        (T, "Live Webinar: MySQL Cluster - Architectural Deep Dive, June 17th 2009",
         Decode ("=?utf-8?B?TGl2ZSBXZWJpbmFyOiBNeVNRTCBDbHVzdGVyIC0gQXJjaGl0ZWN0dXJh"
           & "bCBEZWVwIERpdmUsIEp1bmUgMTd0aCAyMDA5?="),
         "Invalid decode");
   end Test_Decode;

end Mail.Headers.Tests;
