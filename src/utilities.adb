-------------------------------------------------------------------------------
--                                                                           --
--                                CopyPics                                   --
--                                                                           --
--                                Utilities                                  --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Calendar.Formatting;
with Ada.Directories;

package body Utilities is

   -----------------
   --  Add_Slash  --
   -----------------

   function Add_Slash
     (Path : in String)
      return String
   is
   begin
      if Path (Path'Last) /= '/' then
         return Path & "/";
      end if;

      return Path;
   end Add_Slash;

   -----------------
   --  To_String  --
   -----------------

   function To_String
     (Stamp : in Ada.Calendar.Time)
      return String
   is
      use Ada.Calendar;

      S : constant String := Formatting.Image (Stamp);
   begin
      return S (1 .. 10);
   end To_String;

   ------------------
   --  Valid_Path  --
   ------------------

   function Valid_Path
     (Path : in String)
      return Boolean
   is
      use Ada.Directories;
   begin
      if Exists (Path)
        and then Kind (Path) = Directory
      then
         return True;
      end if;

      return False;
   end Valid_Path;

end Utilities;
