-------------------------------------------------------------------------------
--                                                                           --
--                                Picmover                                   --
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
with Ada.Text_IO;
with Help;
with Utilities;

procedure Picmover is
   use Ada.Command_Line;
   use Ada.Directories;
   use Ada.Text_IO;
   use Utilities;

   procedure Copy_JPEG_Files
     (Search_Item : in Directory_Entry_Type);
   --  Copy JPEG files found in the source directory to a directory named
   --  yyyy-mm-dd in target directory. Creates the target yyyy-mm-dd directory
   --  if it doesn't exist.

   function Valid_Path
     (Path : in String)
      return Boolean;
   --  Check if the given Path is valid, ie.:
   --      1. Exists
   --      2. Is a directory
   --      3. Contains 1 or more jpg/jpeg files

   -----------------------
   --  Copy_JPEG_Files  --
   -----------------------

   procedure Copy_JPEG_Files
     (Search_Item : in Directory_Entry_Type)
   is
      Dir_Name : constant String :=
                   To_String (Modification_Time (Search_Item));
   begin
      Put_Line (Simple_Name (Search_Item));
      Put_Line (Dir_Name);
   end Copy_JPEG_Files;

   ------------------
   --  Valid_Path  --
   ------------------

   function Valid_Path
     (Path : in String)
      return Boolean
   is
   begin
      if Exists (Path)
        and then Kind (Name => Path) = Directory
      then
         return True;
      end if;

      return False;
   end Valid_Path;

   Filter : constant Filter_Type := (Ordinary_File => True,
                                     Special_File  => False,
                                     Directory     => False);
   Print_Help : Boolean := False;
begin
   if Argument_Count = 0 or Argument_Count > 2 then
      --  Bad arguments given, print help.
      Print_Help := True;
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

         if Argument (1) = Argument (2) then
            Print_Help := True;
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
end Picmover;
