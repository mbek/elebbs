program FileDn;
(*
**
** EleWEB EleXer source.
** FileDN - Script to help ease the file download.
**
** Created: 18-July-2002
** Last update: 8-August-2002
**
** Todo:
**  - Ratio
**  - Downloadnum, Downloadk ratio, etc
**  
*)


{-- Include standard type definitions ---------------------------------------}
{$I stdrec.inc}

{-- Include area records ----------------------------------------------------}
{$i arearec.inc}

{-- Include file header records ---------------------------------------------}
{$i fhdrrec.inc}


var AreaInf    : FilesRecord;
    EleAreaInf : EleFilesRecord;
    FileHdr    : FilesHdrRecord;
    FileNameStr: String;
    AreaNum    : Integer;
    HdrNum     : Integer;

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

function AccessToArea(AreaNum: Integer; CheckGroup, IsDownload: Boolean): Boolean;
var TmpBool: Boolean;
begin
  {-- Initialize variables --------------------------------------------------}
  TmpBool := TRUE;

  {-- make sure we can list this area ---------------------------------------}
  if NOT GetFilesRecord(AreaInf, AreaNum, TRUE) then
    TmpBool := FALSE;

  {-- Now actually check the access -----------------------------------------}
  if TmpBool then
    if NOT CheckFileAreaAccess(AreaInf,
                               IsDownload,
                               CheckGroup,
                               FALSE,
                               TRUE,
                               FVal(EleCodeStr('|F*'))) then TmpBool := FALSE;

  {-- and return the value --------------------------------------------------}
  AccessToArea := TmpBool;
end; { func. AccessToArea }

procedure SendBinaryFile;
begin
  {-- log an entry -----------------------------------------------------------}
  MakeLogEntry('>', 'Downloading "' + AreaInf.FilePath + fb_GetFileName + '"');

  {-- send a content header --------------------------------------------------}
  web_SendHeader('Content-Disposition:', 'attachment; filename="' + fb_GetFilename + '"');
  web_Openoutput('Content-Type:', 'application/octet-stream');

  {-- and send the contents of this file -------------------------------------}
  fb_DumpBinaryFile(AreaInf.FilePath + fb_GetFileName);
end; { proc. SendBinaryFile }


begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;

  {-- Make sure this is not run on an anonymous logon ------------------------}
  if NOT web_IsLoggedOn then
    begin
      web_RunErrorScript(4, '');              { Show an "Access denied" error }
      Halt;                                           { Terminate this script }
    end; { if }

  {-- First get some variables -----------------------------------------------}
  AreaNum := FVal(web_GetFormData('areanum'));
  HdrNum := FVal(web_GetFormData('recordnum'));

  {-- check if we have (download) access to this area ------------------------}
  if AccessToArea(AreaNum, false, true) then
    begin
      {-- Try to find the actual file ----------------------------------------}
      if fb_OpenFileBase(AreaInf.AreaNum) then
        begin
          {-- we know which record we want -----------------------------------}
          if fb_Read(HdrNum) then ;

          {-- now make sure the file is not unlisted or unavail --------------}
          if NOT fb_UnlistedFile then
            begin
              if NOT fb_IsNotAvail then
                begin
                  SendBinaryFile;
                end { if }
                  else MakeLogEntry('!', 'File is not available, download aborted');
            end
              else MakeLogEntry('!', 'File is unlisted, download aborted');
        end { if }
          else MakeLogEntry('!', 'Unable to open filebase, for area #' + FStr(AreaNum));
	 
      {-- close the filebase ------------------------------------------------------}
      fb_CloseFileBase;    
    end { if }
      else MakeLogEntry('!', 'Access denied for area #' + FStr(AreaNum));
  

end. { dnfile }
