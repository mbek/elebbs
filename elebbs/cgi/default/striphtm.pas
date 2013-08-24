program StripHtml;
(*
**
** StripHtml - Program to strip HTML files to be as small as possible
** How smaller the HTML is, how smaller the loops in EleWEB are, and
** loops are expensive.
**
** Create date: 28-Jul-2002
** Last update: 28-Jul-2002
** Author: Maarten Bekers
**
*)

var
  Error: Integer;


procedure ShowHelp;
begin
  WriteLn('Usage:');
  WriteLn;
  WriteLn('eleweb.exe striphtm "dir1=<source dir>&dir2=<destination dir>"');
  WriteLn;
  WriteLn('NOTE: Source dir and destination dir MUST be different');
  WriteLn;
end; { proc. ShowHelp }

procedure CompressFile(FName: String);
var HtmlHandle: Integer;
    NewFile   : Integer;
    Error     : Integer;
    DataStr   : String;
    CollateStr: String;
    DoWrite   : Boolean;
    TmpPos    : Integer;
    TempCopy  : String;
begin
  {-- Initialize variables --------------------------------------------------}
  CollateStr := '';

  {-- Assign the file to the proper handle etc ------------------------------}
  HtmlHandle := FileAssign(ForceBack(web_GetFormData('dir1')) + FName, 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  {-- Assign the file to the proper handle etc ------------------------------}
  NewFile := FileAssign(ForceBack(web_GetFormData('dir2')) + FName, 2, 3);
  Error := FileGetError(NewFile);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileCreate(NewFile);
  Error := FileGetError(NewFile);

  if (Error = 0) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         {-- Read the next line ---------------------------------------------}
         FileReadStringLn(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- Trim the string ------------------------------------------------}
         DataStr := Trim(DataStr);

         {-- do some silly string thingy ------------------------------------}
         CollateStr := CollateStr + ' ' + DataStr;

         {-- Remove any comment lines ---------------------------------------}
         TmpPos := Pos('<!--', CollateStr);
         if TmpPos > 0 then
           begin
             TempCopy := Copy(CollateStr, TmpPos, Length(CollateStr));
             if Pos('-->', TempCopy) > 0 then
               begin
                 Delete(CollateStr, TmpPos, Pos('-->', TempCopy) + 2);
               end; { if }
           end; { if }

         {-- and write it to the new file -----------------------------------}
         if Length(CollateStr) > 500 then
           begin
             FileWriteStringLn(NewFile, CollateStr);
             CollateStr := '';
           end; { if }
       end; { while }
   end; { if }

  {-- write the left over ---------------------------------------------------}
  if Length(CollateStr) > 0 then
    begin
      FileWriteStringLn(NewFile, CollateStr);
    end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(HtmlHandle);
  FileClose(NewFile);
  FileDone(HtmlHandle);
  FileDone(NewFile);
end; { proc. CompressFile }

begin
  {-- Show our banner -------------------------------------------------------}
  WriteLn('STRIPHTM - Strip HTML files');
  WriteLn('(c)2002 by Maarten Bekers');
  WriteLn;

  {-- Now sanity check on parameters ----------------------------------------}
  if ((web_GetFormData('dir2') = '') OR
      (web_GetFormData('dir1') = '')) OR
       (sLowCase(ForceBack(web_GetFormData('dir1'))) = ForceBack(sLowCase(web_GetFormData('dir2')))) then
    begin
      ShowHelp;
    end
      else begin
             {-- Find all html files in this directory ----------------------}
             Error := FindFirst(1, ForceBack(web_GetFormData('dir1')) + '*.*', 37);

             {-- loop -------------------------------------------------------}
             while (Error = 0) do
               begin
                 {-- Show something -----------------------------------------}
                 Write('Processing ', FindGetName(1));

                 {-- Compress the file --------------------------------------}
                 CompressFile(FindGetName(1));

                 {-- update the screen --------------------------------------}
                 WriteLn(', done.');

                 {-- and find the next file ---------------------------------}
                 Error := FindNext(1);
               end; { while }


             {-- and close --------------------------------------------------}
             FindClose(1);
           end; { else }
end.
