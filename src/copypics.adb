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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Help;
with Utilities;

procedure CopyPics is

   use Ada.Command_Line;
   use Ada.Directories;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Utilities;

   type CLI_Args is
      record
         Delete_Source_Files : Boolean := False;
         Print_Help          : Boolean := False;
         Source_Dir          : Unbounded_String := Null_Unbounded_String;
         Target_Dir          : Unbounded_String := Null_Unbounded_String;
      end record;

   Args         : CLI_Args;
   Copy_Counter : Natural := 0;
   Del_Counter  : Natural := 0;
   Filter       : constant Filter_Type := (Ordinary_File => True,
                                           Special_File  => False,
                                           Directory     => False);

   procedure Copy_Files
     (Search_Item : in Directory_Entry_Type);
   --  Copy JPEG/RAW files found in the source directory to a directory named
   --  yyyy-mm-dd in target directory. Creates the target yyyy-mm-dd directory
   --  if it doesn't exist.
   --  Deletes the source file if the -d / --delete commandline parameter is
   --  given.

   ------------------
   --  Copy_Files  --
   ------------------

   procedure Copy_Files
     (Search_Item : in Directory_Entry_Type)
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;

      Dir_Name  : constant String :=
                    To_String (Modification_Time (Search_Item));
      --  The yyyy-mm-dd string.

      File_Name : constant String := Simple_Name (Search_Item);
      File_Ext  : constant String := Translate (Extension (File_Name),
                                                Lower_Case_Map);
      Target_Dir : constant String := To_String (Args.Target_Dir) & Dir_Name;
   begin
      if File_Ext = "jpg"
        or File_Ext = "jpeg"
        or File_Ext = "raw"
        or File_Ext = "arw"
      then
         if not Exists (Target_Dir) then
            Create_Directory (Target_Dir);
         end if;

         Handle_File :
         declare
            Source : constant String := Full_Name (Search_Item);
            Target : constant String := Compose (Target_Dir, File_Name);
         begin
            Put ("Copying " & File_Name & " to " & Target_Dir);
            Copy_File (Source, Target, "preserve=timestamps");
            if Exists (Target) then
               Put (" - Success!");
               Copy_Counter := Copy_Counter + 1;

               if Args.Delete_Source_Files then
                  Delete_File (Source);
                  New_Line;
                  if Exists (Source) then
                     Put ("Could not delete " & Source);
                  else
                     Put ("Deleted " & Source);
                     Del_Counter := Del_Counter + 1;
                  end if;
               end if;
            else
               Put (" - Failed!");
            end if;
            New_Line;
         end Handle_File;
      end if;
   end Copy_Files;
begin
   if Argument_Count < 2 or Argument_Count > 3 then
      --  Bad arguments. Print help.
      Args.Print_Help := True;
   end if;

   Verify_Arguments :
   for k in 1 .. Argument_Count loop
      if Argument (k) = "-h" or Argument (k) = "--help" then
         Args.Print_Help := True;
         exit Verify_Arguments;
      elsif Argument (k) = "-d" or Argument (k) = "--delete" then
         Args.Delete_Source_Files := True;
      else
         if not Valid_Path (Path => Argument (k)) then
            Args.Print_Help := True;
            exit Verify_Arguments;
         else
            --  At this point we've got a valid path, so lets add it to our
            --  Args object.
            if Args.Source_Dir = Null_Unbounded_String then
               Args.Source_Dir := To_Unbounded_String
                 (Add_Slash (Argument (k)));
            elsif Args.Target_Dir = Null_Unbounded_String then
               Args.Target_Dir := To_Unbounded_String
                 (Add_Slash (Argument (k)));
            else
               --  Source and target already set. One path too many!
               Args.Print_Help := True;
            end if;
         end if;
      end if;
   end loop Verify_Arguments;

   if Args.Source_Dir = Args.Target_Dir then
      Args.Print_Help := True;
   end if;

   if Args.Print_Help then
      Help.Print;
   else
      Search (Directory => To_String (Args.Source_Dir),
              Pattern   => "",
              Filter    => Filter,
              Process   => Copy_Files'Access);

      if Copy_Counter > 0 then
         New_Line;
      end if;
      Put_Line ("Copied" & Natural'Image (Copy_Counter) & " files");
      Put_Line ("Deleted" & Natural'Image (Del_Counter) & " files");
   end if;

end CopyPics;
