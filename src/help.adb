-------------------------------------------------------------------------------
--                                                                           --
--                                CopyPics                                   --
--                                                                           --
--                                  Help                                     --
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

with Ada.Text_IO;

package body Help is

   use Ada.Text_IO;

   -------------
   --  Print  --
   -------------

   procedure Print
   is
   begin
      Put_Line ("Usage: copypics [options] SOURCE TARGET");
      Put_Line ("Options:");
      Put_Line ("    -h | --help for this text.");
      Put_Line ("    -d | --delete to delete source files after copying.");
      Put_Line ("Example:");
      Put_Line ("    copypics -d /source/dir/ /target/dir/");
      New_Line;
      Put_Line ("CopyPics copies JPEG/RAW files from the given SOURCE");
      Put_Line ("directory to the given TARGET directory. Directories named");
      Put_Line ("yyyy-mm-dd are created and populated in the TARGET");
      Put_Line ("directory according to the modification time of each file");
      Put_Line ("found in the SOURCE directory. The SOURCE and TARGET");
      Put_Line ("directory must not be the same.");
   end Print;

end Help;
