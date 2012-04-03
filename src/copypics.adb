-------------------------------------------------------------------------------
--                                                                           --
--                                CopyPics                                   --
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
--  with Ada.Text_IO;
with Help;
with Utilities;

procedure CopyPics is

   use Ada.Command_Line;
   use Ada.Directories;
   --  use Ada.Text_IO;
   use Utilities;

   procedure Copy_JPEG_Files
     (Search_Item : in Directory_Entry_Type);
   --  Copy JPEG files found in the source directory to a directory named
   --  yyyy-mm-dd in target directory. Creates the target yyyy-mm-dd directory
   --  if it doesn't exist.

   -----------------------
   --  Copy_JPEG_Files  --
   -----------------------

   procedure Copy_JPEG_Files
     (Search_Item : in Directory_Entry_Type)
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;

      Dir_Name  : constant String :=
                    To_String (Modification_Time (Search_Item));
      File_Name : constant String := Simple_Name (Search_Item);
      File_Ext  : constant String := Translate (Extension (File_Name),
                                                Lower_Case_Map);
      Target_Dir : constant String := Add_Slash (Argument (2)) & Dir_Name;
   begin
      --  First we check if this is indeed a valid JPEG file. This is done by
      --  checking the filename for either .jpg or .jpeg. We do not care about
      --  case.
      if File_Ext = "jpg" or File_Ext = "jpeg" then
         --  Check if the target folder yyyy-mm-dd exists and create it if it
         --  doesn't.
         if not Exists (Target_Dir) then
            Create_Directory (Target_Dir);
         end if;

         --  Copy the file.
         Copy_File (Full_Name (Search_Item), Compose (Target_Dir, File_Name));
      end if;
   end Copy_JPEG_Files;

   Filter : constant Filter_Type := (Ordinary_File => True,
                                     Special_File  => False,
                                     Directory     => False);
   Print_Help : Boolean := False;
begin
   --  If we've got bad arguments, then print help.
   if Argument_Count /= 2 then
      Print_Help := True;
   else
      if Add_Slash (Argument (1)) = Add_Slash (Argument (2)) then
         Print_Help := True;
      end if;
   end if;

   Verify_Arguments :
   for k in 1 .. Argument_Count loop
      if Argument (k) = "-h" or Argument (k) = "--help" then
         Print_Help := True;
         exit Verify_Arguments;
      else
         if not Valid_Path (Path => Argument (k)) then
            Print_Help := True;
            exit Verify_Arguments;
         end if;
      end if;
   end loop Verify_Arguments;

   if Print_Help then
      Help.Print;
   else
      Search (Directory => Argument (1),
              Pattern   => "",
              Filter    => Filter,
              Process   => Copy_JPEG_Files'Access);
   end if;

end CopyPics;
