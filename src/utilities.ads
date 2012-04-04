-------------------------------------------------------------------------------
--                                                                           --
--                                CopyPics                                   --
--                                                                           --
--                                Utilities                                  --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                      Copyright (C) 2012-, Thomas Løcke                    --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Calendar;

package Utilities is

   function Add_Slash
     (Path : in String)
      return String;
   --  Postfix a forward slash to Path, if it is missing.

   function To_String
     (Stamp : Ada.Calendar.Time)
      return String;
   --  Return a yyyy-mm-dd formatted string based on Stamp.

   function Valid_Path
     (Path : in String)
      return Boolean;
   --  Check if the given Path is valid, ie.:
   --      1. Exists
   --      2. Is a directory

end Utilities;
