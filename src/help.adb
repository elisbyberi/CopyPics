-------------------------------------------------------------------------------
--                                                                           --
--                                Picmover                                   --
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
      Put_Line ("Usage: picmover /path/to/source/dir /path/to/target/dir");
      Put_Line ("       picmover -h | --help for this text.");
      New_Line;
      Put_Line ("Moves JPEG files from source directory to target directory.");
      Put_Line ("Directories named yyyy-mm-dd are created and populated in");
      Put_Line ("the target directory according to the modification time of");
      Put_Line ("each JPEG file found in the source directory.");
      Put_Line ("Source and target directory must not be the same.");
   end Print;

end Help;
