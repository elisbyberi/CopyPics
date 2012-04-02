-------------------------------------------------------------------------------
--                                                                           --
--                                Picmover                                   --
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

package body Utilities is
   -----------------
   --  To_String  --
   -----------------

   function To_String
     (Stamp : in Ada.Calendar.Time)
      return String
   is
      use Ada.Calendar;

      Y : constant String := Year_Number'Image (Year (Stamp));
      M : constant String := Month_Number'Image (Month (Stamp));
      D : constant String := Day_Number'Image (Day (Stamp));
   begin
      --      return Formatting.Image (Stamp);
      return Y (Y'First + 1 .. Y'Last) & M & D;
   end To_String;
end Utilities;
