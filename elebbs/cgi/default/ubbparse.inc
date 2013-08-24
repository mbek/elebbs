(*
** UBB code parser
** Smiley parser
**
** Code file for EleXer
**
** Written: 14-Jul-2002
**
**)

function GrabURL(S: String; var Start: Longint):String;
var TempStr: String;
    TempPos: Longint;
begin
  TempPos := Start - 1;

  While (TempPos > 00) AND (TempPos <= Length(s)) AND
        (S[TempPos] <> ':') AND
         ((Ord(UpCase(S[TempPos])) >= Ord('A')) AND
          (Ord(UpCase(S[TempPos])) <= Ord('Z'))) OR
           ( (S[TempPos] = '_') OR (S[TempPos] = '.') OR
             (S[TempPos] = '-') OR (S[TempPos] = '&') OR
             (S[TempPos] = '+') ) do
              TempPos := TempPos - 1;

  TempPos := TempPos + 1;
  Start := TempPos;

  TempStr := '';
  while (TempPos <= Length(S)) do
    begin
      if (Pos(S[TempPos],' ^*(){}<>[]|;"''') > 0) then
        TempPos := Length(s) + 1
          else begin
                 TempStr := TempStr + S[TempPos];
                 TempPos := TempPos + 1;
               end; { else }
    end; { while }

  GrabURL := TempStr;
end; { func. GrabURL }


function LegalAddress(Addr: String):Boolean;
var Name   : String;
    Server : String;
    DoContinue: Boolean;
begin
  DoContinue := true;

  Name   := Copy(Addr, 1, Pos('@', Addr));
  Server := Addr;

  Delete(Server, 1, Length(Name) + 01);

  if (Server = '') OR (Name = '') OR (Name = '@') then DoContinue := false;
  if Pos('@', Server) > 00 then DoContinue := false;
  if Pos(#32, Name) > 00 then DoContinue := false;
  if Pos(#32, Server) > 00 then DoContinue := false;
  if Pos('.', Server) = 0 then DoContinue := false;

  LegalAddress := DoContinue;
end; { func. LegalAddress }


function ToWWW(S: String): String;
var Start : Longint;
    PosCnt: Longint;
    URL   : String;
begin
  Url := SUpCase(s);
  PosCnt := Pos('@', S);

  if PosCnt > 00 then
    begin
      URL := GrabURL(S, PosCnt);
      Delete(S, PosCnt, length(Url));

      if LegalAddress(url) then
        begin
          Url := '[url=mailto:' + Url + ']' + Url + '[/url]';
        end; { if }

      Insert(Url, S, PosCnt);
    end; { if }

  PosCnt := Pos('://', Url);
  if PosCnt > 00 then
    begin
      Url := GrabURL(S, PosCnt);
      Delete(S, PosCnt, Length(Url));

      Url :='[url='+ Url + ']' + Url + '[/url]';
      Insert(Url, S, PosCnt);
    end; { if }

  ToWWW := S;
end; { func. ToWww }



procedure InitUbbParser;
begin
  ubb_AllowColors := true;
  ubb_AllowImg := true;
end; { proc. InitUbbParser }



procedure AddToTable(Typ: Char; TmpStr: String);
begin
  if Typ='p' then
    TableCount := TableCount + 1;

  if typ = 'p' then
    smiley_Pic[TableCount] := TmpStr
     else smiley_Code[TableCount] := TmpStr;
end; { proc. AddToTable }

procedure InitTable;
begin
  TableCount := 0;

  AddToTable('p', '<img src="/images/devil.gif" width="15" height="15">');
  AddToTable('s', '&gt;:)');

  AddToTable('p', '<img src="/images/biggrin.gif" width="15" height="15">');
  AddToTable('s', ':D');

  AddToTable('p', '<img src="/images/wink.gif" width="15" height="15">');
  AddToTable('s', ';)');

  AddToTable('p', '<img src="/images/clown.gif" width="15" height="15">');
  AddToTable('s', ':+');

  AddToTable('p', '<img src="/images/smile.gif" width="15" height="15">');
  AddToTable('s', ':)');

  AddToTable('p', '<img src="/images/frown.gif" width="15" height="15">');
  AddToTable('s', ':(');

  AddToTable('p', '<img src="/images/redface.gif" width="15" height="15">');
  AddToTable('s', ':O');

  AddToTable('p', '<img src="/images/3d-crying.gif" width="20" height="15">');
  AddToTable('s', ':''(');

  AddToTable('p', '<img src="/images/puh.gif" width="15" height="15">');
  AddToTable('s', ':P');
end; { proc. AddToTable }


procedure FixSmileys(var TmpStr: String);
begin
  ReplaceArray(TmpStr, Smiley_Code, Smiley_Pic, false);
end; { proc. FixSmileys }



function UbbParse(InputStr: String; Nick: String): String;
const
  MaxRecurs = 15;    { we limit the maximal recursives to 15 for now }
  
var BrackPos  : Integer;                              { last found [ } 
    MyLevel   : Integer;        { the amount of tags were processing }
    OutputStr : String;                      { the html-ified string }
    TempBool  : Boolean;
    
    CodeCount : Integer;                { Amount of nested code tags }
    QuoteCount: Integer;               { Amount of nested quote tags }
    NoUbbCount: Integer;               { Amount of nested noubb tags }

    Html_Codes   : Array[1..4] of String;
    Html_Escaped : Array[1..4] of String;
    
procedure Ubb2Html(Codetag, CodeSep, CodeParams, TagContents: String; var ubbOpen, ubbClose: String);

function AllowedTag: Boolean;
begin                    
  {-- by default we allow all tags ----------------------------------}
  AllowedTag := TRUE;
  
  {-- if were in a code statement, invalidate all previous codes ----}
  if CodeCount > 0 then
    begin         
      AllowedTag := false;
      
      ubbOpen := '[' + CodeTag + CodeSep + CodeParams + ']';
      ubbClose := '[/' + CodeTag + ']';
    end { if }

    {-- if were in a quote statement, invalidate all previous quotes-}
    else if QuoteCount > 0 then 
          begin
            if CodeTag = 'quote' then
             begin
               AllowedTag := false;
      
               ubbOpen := '[...]';             
             end; { if }
          end { if }

    {-- if an img tag, and we dont allow images ---------------------}
    else if NOT ubb_AllowImg then 
           begin
             if CodeTag = 'img' then
               begin
                 AllowedTag := false;
        
                 ubbOpen := '[' + CodeTag + CodeSep + CodeParams + ']';
                 ubbClose := '[/' + CodeTag + ']';
               end; { if }
           end; { if }
end; { func. AllowedTag }

begin
  {-- Initialize variables ------------------------------------------}
  ubbOpen := '';
  ubbClose := '';

  {-- and actually interpret the code -------------------------------}
  case CodeTag[1] of 
    '*' : begin
    	    if AllowedTag then
    	      ubbOpen := '<li>';
    	  end; { [*] }
    
    'b' : if CodeTag = 'b' then
            begin
              if AllowedTag then
                begin
                  ubbOpen := '<b>';
                  ubbClose := '</b>';
                end; { if }  
            end { if }
            
           else if CodeTag = 'br' then
             begin         
               if AllowedTag then
                 begin
                   ubbOpen := '<br>';
                 end; { if }
             end { if }
             
           else if CodeTag = 'bgcolor' then
             begin          
               if AllowedTag then
                 begin
                   ubbOpen := '<FONT STYLE="background-color: ' + CodeParams + '>';
                   ubbClose := '</FONT>';
                 end; { if }
             end; { if }
             
    'c' : if CodeTag = 'c' then
            begin           
              if AllowedTag then
                begin
                  ubbOpen := '<c>';
                  ubbClose := '</c>';
                end; { if }
            end
           
           else if CodeTag = 'color' then
             begin            
               if AllowedTag then
                 begin
                   ubbOpen := '<FONT COLOR="' + CodeParams + '">';
                   ubbClose := '</FONT>';
                 end; { if }
             end { if }
             
           else if CodeTag = 'code' then
             begin          
               if AllowedTag then
                 begin
                   ubbOpen := '<pre><blockquote><font class=code_hdr>code</font>: ' + 
                                 CodeParams + '<HR noshade size=1>';
                   ubbClose := '<HR noshade size=1></blockquote></pre>';
                 end; { if }
             end; { if }
             
    'h' : if CodeTag = 'hr' then
            begin           
              if AllowedTag then
                begin
                  ubbOpen := '<hr>';
                end; { if }
    	    end; { [hr] }
               	  
    'u' : if CodeTag = 'u' then
            begin          
              if AllowedTag then
                begin
                  ubbOpen := '<u>';
                  ubbClose := '</u>';
                end; { if }
    	    end { [u] }
    	   
    	   else if CodeTag = 'ul' then
    	     begin         
    	       if AllowedTag then
    	         begin	 
                   ubbOpen := '<ul>';
    	           ubbClose := '</ul>';	
    	         end; { if }
    	     end { [ul] }

    	   else if CodeTag = 'ulist' then
    	     begin         
    	       if AllowedTag then
    	         begin	 
                   ubbOpen := '<ul>';
    	           ubbClose := '</ul>';	
    	         end; { if }
    	     end { [ulist] }

    	   else if CodeTag = 'url' then
    	     begin          
    	       if AllowedTag then
    	         begin	             
    	           { use the tag contents as tagvalue }
    	           if CodeParams = '' then
    	             CodeParams := TagContents;
    	          
    	           { correct the url if necessary }
    	           if Pos('://', CodeParams) = 0 then
    	             CodeParams := 'http://' + CodeParams;                          
    	                                     	                                 
    	           ubbOpen := '<a href="' + CodeParams + '">';
    	           ubbClose := '</a>';	
    	         end; { if }  
    	     end; { [url] }

    'i' : if CodeTag = 'i' then
            begin           
              if AllowedTag then
                begin	
                  ubbOpen := '<i>';
                  ubbClose := '</i>';
                end; { if } 
    	    end { [i] }

    	   else if CodeTag = 'img' then
    	     begin         
    	       if AllowedTag then
    	         begin	
    	           ubbOpen := '<img border=0 src="' + CodeParams + '">';
    	           ubbClose := '</img>';	
    	         end; { if }  
    	     end; { [img] }
                   	  
    's' : if CodeTag = 's' then
            begin           
              if AllowedTag then
                begin	 
                  ubbOpen := '<s>';
                  ubbClose := '</s>';
                end; { if }  
    	    end { [s] }
    	    
    	   else if CodeTag = 'sup' then
    	     begin         
    	       if AllowedTag then
    	         begin	 
    	           ubbOpen := '<sup>';
    	           ubbClose := '</sup>';	
    	         end; { if }  
    	     end { [sup] }
    	     
    	   else if CodeTag = 'sub' then
    	     begin          
    	       if AllowedTag then
    	         begin	
    	           ubbOpen := '<sub>';
    	           ubbClose := '</sub>';	
    	         end; { if }  
    	     end; { [sub] }
    	    
    'q' : if CodeTag = 'quote' then
            begin           
              if AllowedTag then
                begin	
                  ubbOpen := '<blockquote><font class=quote_hdr>quote:</font>' + 
                              '<HR noshade size=1>';
                  ubbClose := '<HR noshade size=1></blockquote>';
                end; { if }
    	    end; { [quote] }

    'l' : if CodeTag = 'list' then
            begin          
              if AllowedTag then
                begin	
                  if CodeParams = '' then
                    begin
                      ubbOpen := '<ul>';
                      ubbClose := '</ul>';
                    end
                      else begin
                             ubbOpen := '<ol TYPE="' + CodeParams + '">';
                  	     ubbClose := '</ol>';
                           end; { else }
                end { if }           
    	    end { [list] }

    	   else if CodeTag = 'li' then
    	     begin          
    	       if AllowedTag then
    	         begin	
    	           ubbOpen := '<li>';
    	           ubbClose := '</li>';	
    	         end; { if }  
    	     end; { [li] }
    	    
    else ;    
  end; { case }
end; { prob. Ubb2Html }

procedure ParseTag(var MyLevel     : Integer; 
                   var BrackPos    : Integer;
                       LookClosing : String;
                   var WasValid    : Boolean);
                   
const
  noCloseExceptionTags = ',*,br,hr';
  ValidUbbTags         = ',*,b,br,bgcolor,c,color,code,hr,u,ul,ulist,url,i,img,s,sup,sub,quote,list,li,';
    
                     
var DoStop       : Boolean;   { we found an error or a closing bracket }
    UbbCodeStr   : String;                           { actual UBB code }
    UbbCodeParams: String;          { and the parameters for that code }
    UbbCodeSep   : String;                        { UBBcode seperators }
    SaveUbbCode  : String;
    SaveUbbParams: String;{ and the saved parameter for the saved code }
    SaveUbbSep   : String;                             { UBB seperator }
    Counter      : Integer;              { where in the string are we? }
    ClosePos     : Integer;             { find the closing bracket (]) }
    OpenPos      : Integer;                 { and find an open bracket }

    ubbCode_Close,                        
    ubbCode_Open   : String;
begin
  {-- initialize some variables ----------------------------------------------}	
  DoStop := false;	
  MyLevel := MyLevel + 1;
  WasValid := false;

(*
if lookclosing <> '' then
  writeln('looking for clsoing..: "',lookclosing,'"');
*)  

  {-- now loop through the string --------------------------------------------}
  while (InputStr <> '') AND (NOT DoStop) do
    begin
      {-- now, try to find the first UBB code --------------------------------}
      BrackPos :=  Pos('[', InputStr);

      {-- if no more UBB codes are found, assume were finished ---------------}
      if BrackPos = 0 then
        begin
          BrackPos := Length(InputStr);
          DoStop := true;
        end
          else BrackPos := BrackPos - 1;

      {-- now everything that doesnt have a [, we skip -----------------------}
      {-- unless the user is quoting nested - we replace the nested quotes ---}
      {-- with an [...] and just delete the nested-quoted text ---------------}
      if QuoteCount < 2 then
        begin
          Concatenate(OutputStr, Copy(InputStr, 1, BrackPos)); 
        end; { if }

      {-- and delete the non-macro code --------------------------------------}
      Delete(InputStr, 1, BrackPos);

      {-- now we actively start to parse the tag -----------------------------}
      if NOT DoStop then
        begin
          {- we just delete the opening [ ------------------------------------}
          {-- its a lot easier, else we have to (in the next routines) -------}
          {-- compensate for the first opening bracket -----------------------}
          Delete(InputStr, 1, 1);
          
          {-- first we search for a closing bracket, if it doesnt exist, we --}
          {-- just assume the whole string is non-ubb code and ignore it. If -}
          {-- we do find one, first search for another opening bracket. ------}
          {-- If a closingone comes first its closing for another tag, and ---}
          {-- we have to ignore it all together ------------------------------}
          ClosePos := Pos(']', InputStr);
          OpenPos := Pos('[', InputStr);
          if ClosePos = 0 then
            begin
              ClosePos := Length(InputStr);
            end; { if }

          {-- if the opening tag comes before this new tag, invalidate it ----}
          if OpenPos > 0 then
           if OpenPos < ClosePos then
             begin
               {-- update the closepos ---------------------------------------}
               ClosePos := OpenPos;
              
               {-- and add the invalid code to the string --------------------}
               {-- because we deleted the opening [ first, we have to add it -}
               {-- here ------------------------------------------------------}
               Concatenate(OutputStr, '[' + Copy(InputStr, 1, ClosePos - 1));

               {-- and delete the non-macro code -----------------------------}
               Delete(InputStr, 1, ClosePos);
              
               {-- now we have to correct the closepos -----------------------}
               {-- because we just modified the string -----------------------}
               ClosePos := Pos(']', InputStr);              
            end; { if invalid }  

          {-- extract the UBB code -------------------------------------------}
          UbbCodeStr := Copy(InputStr, 1, ClosePos - 1);
          
          {-- and delete it from the input string ----------------------------}
          Delete(InputStr, 1, ClosePos);
          
          {-- now split the ubbcode str --------------------------------------}
          ClosePos := Pos(' ', UbbCodeStr);
          Counter := Pos('=', UbbCodeStr);
          if (ClosePos = 0) OR (Counter < ClosePos) then
            begin                 
              UbbCodeSep := '=';	
              ClosePos := Counter;
            end
              else UbbCodeSep := ' ';
            
          {-- and actually split the string ----------------------------------}
          if ClosePos > 0 then
            begin
              UbbCodeParams := Copy(UbbCodeStr, ClosePos + 1, Length(UbbCodeStr));
              Delete(UbbCodeStr, ClosePos, Length(UbbCodeStr));
            end { if }
              else begin
              	     UbbCodeParams := '';
              	     UbbCodeSep := '';
              	   end; { else }  
        end; { if }  

      {-- we recursively parse all those tags --------------------------------}
      if NOT DoStop then
       if Copy(UbbCodeStr, 1, 1) <> '/' then
        begin
          {-- now try to parse the remaining text ----------------------------}
          Counter := Length(OutputStr);
            
          {-- now we save this value -----------------------------------------}
          SaveUbbCode := UbbCodeStr;
          SaveUbbParams := UbbCodeParams;
          SaveUbbSep := UbbCodeSep;
            	         
          {-- if this is a special tag, start handling it --------------------}
          if SaveUbbCode = 'code' then
            begin                     
              CodeCount := CodeCount + 1;
            end { if }
           else
             if SaveUbbCode = 'quote' then
               begin
               	  QuoteCount := QuoteCount + 1;
               end { if }
           else
             if SaveUbbCode = 'noubb' then
               begin
                 NoUbbCount := NoUbbCount + 1;
               end; { if }
            
          {-- and try to parse the new tag -----------------------------------}
          {-- but only parse if, it we expect a closing tag ------------------}
          if NoUbbCount = 0 then
            begin
              if (Pos(',' + UbbCodeStr + ',', NoCloseExceptionTags) = 0) then
                begin                                                              
                  {-- check if this is a valid tag ---------------------------}
                  if (Pos(',' + UbbCodeStr + ',', ValidUbbTags) <> 0) then 
                    begin
                      {-- and now parse the whole tag ------------------------}
                      if MyLevel < MaxRecurs then
                        ParseTag(MyLevel, BrackPos, UbbCodeStr, WasValid);
                    end
                      else begin        
                             {-- re-add this tag - its invalid ubb -----------}
                             Insert('[' + SaveUbbCode + SaveUbbSep + SaveUbbParams + ']', 
                 	             OutputStr, Counter + 1);

                         {-- and mark this tag as valid ----------------------}
                         WasValid := true;
                       end; { else }            
                end
                  else WasValid := true;
            end
              else begin      
                     WasValid := false;
                   end; { if }      

          {-- if this is a special tag, start handling it --------------------}
          {-- we already icnreased the counts above, but if the tag -- -------}
          {-- after handling, appears to be invalid, we undo the count ---- --}
          if NOT WasValid then
            begin
              if SaveUbbCode = 'code' then
                begin                     
                  CodeCount := CodeCount - 1;
                end { if }                 
               else 
                 if SaveUbbCode = 'quote' then
                   begin
                     QuoteCount := QuoteCount - 1;
                   end; { if }
            end; { if }       

{writeln('wasvalid ===> ', WasValid, ' ---> SaveUbbCode=', SaveUbbCode, ', CodeCount = ', CodeCount,'<br>');}
            
          {-- and add the closing tag ----------------------------------------}          
          if WasValid then
            begin
               if SaveUbbCode <> '' then
                 begin
                  Counter := Counter + 1;

                  {-- convert the UBB tag to HTML ----------------------------}
                  Ubb2Html(SaveUbbCode, SaveUbbSep, SaveUbbParams,  
                            Copy(OutputStr, Counter, Length(OutputStr)), 
                            UbbCode_Open, UbbCode_Close);
            	
                  {-- and add those tags -------------------------------------}
                  Insert(UbbCode_Open, OutputStr, Counter);
                  Concatenate(OutputStr, UbbCode_Close);
                 end; { if }
            end { if }     
              else begin
              	     {-- opening code was never closed, add the opening code -}
              	     if (NoUbbCount > 0) AND ((SaveUbbCode <> 'noubb') OR (NoUbbCount > 1)) then
              	       Insert('[' + SaveUbbCode + SaveUbbSep + SaveUbbParams + ']', 
              	               OutputStr, Counter + 1);
              	    end; { if }

          {-- reset the valid tag --------------------------------------------}
          WasValid := false;          
        end { if }
         else begin
         	{-- we found a closing tag -----------------------------------}
         	{-- first we fix it up ---------------------------------------}
         	Delete(UbbCodeStr, 1, 1); 
                  
                
                {-- we require our tags to be fully closed -------------------}         	
                {-- in the correct way ---------------------------------------}
         	if UbbCodeStr <> LookClosing then
          	  begin                                         
          	    if NOT WasValid then
          	     if (NoUbbCount <> 1) OR (UbbCodeStr <> 'noubb') then
         	      Concatenate(OutputStr, '[/' + UbbCodeStr + ']'); 
                  end
         	   else begin
         	   	   WasValid := true;

                           {-- we found a closing tag ------------------------}
                           if MyLevel > 0 then
                             DoStop := true;                  

         	   	end; { else }
                {-- we have special tags that allow nesting - fix those first-}
                if UbbCodeStr = 'code' then
                  begin
                    if CodeCount > 0 then
                      CodeCount := CodeCount - 1;
                  end { if }
                 else
                   if UbbCodeStr = 'quote' then
                     begin
                       if QuoteCount > 0 then
                         QuoteCount := QuoteCount - 1;
                     end { if }
                 else
                   if UbbCodeStr = 'noubb' then
                     begin
                       if NoUbbCount > 0 then
                         NoUbbCount := NoUbbCount - 1;
                     end; { if }
                
              end; { if }
    end; { while }

  {-- and decrease one level -------------------------------------------------}
  MyLevel := MyLevel - 1;
end; { proc. ParseTag }

begin
  {-- initialize variables ---------------------------------------------------}	
  OutputStr := '';
  CodeCount := 0;
  QuoteCount := 0;
  NoUbbCount := 0;

  {-- special fix up for /me -----------------------------------------------}
  if (Copy(InputStr, 1, 4) = '/me ') then
    begin
      Delete(InputStr, 1, 3);

      Insert('[/me]', InputStr, 1);
    end; { if }

  {-- escape HTML if necessary ----------------------------------------------}
  BrackPos := Pos('<', InputStr);
  if BrackPos = 0 then
    BrackPos := Pos('>', InputStr);
  if BrackPos = 0 then
    BrackPos := Pos(#13, InputStr);

  if BrackPos > 0 then
    if (NOT ubb_AllowHtml) OR (Pos(#13, InputStr) > 0) then
      begin
        {-- initialize array ------------------------------------------------}
        Html_Codes[1] := '<';
        Html_Escaped[1] := '&lt;';

        Html_Codes[2] := '>';
        Html_Escaped[2] := '&gt;';

        Html_Codes[3] := #13;
        Html_Escaped[3] := '<br>';

        Html_Codes[4] := #141;    { "soft" wordwrap }
        Html_Escaped[4] := ' ';

        {-- and replace it --------------------------------------------------}
        ReplaceArray(InputStr, Html_Codes, Html_Escaped, false);
      end; { if }


  {-- initialize variables ---------------------------------------------------}	
  BrackPos := Pos('[', InputStr);
  
  {-- first we scan if there are any ubb codes in this, if not, we dont bother}
  {-- with this string at all ------------------------------------------------}
  if BrackPos = 0 then
    begin
      OutputStr := InputStr;
    end; { if }
    
  {-- keep looping through the string until we processed the whole string ----}
  if BrackPos > 0 then
    begin
      {-- now everything that doesnt have a [, we skip -----------------------}
      Concatenate(OutputStr, Copy(InputStr, 1, BrackPos - 1));  
  
      {-- and delete the non-macro code --------------------------------------}
      Delete(InputStr, 1, BrackPos - 1); 

      {-- and parse the tag --------------------------------------------------}
      ParseTag(MyLevel, BrackPos, '', TempBool);
      
      {-- and if any thing is left over, ignore it ---------------------------}
      Concatenate(OutputStr, InputStr); 
    end; { while }
  
  {-- and return our new htmlized string -------------------------------------}
  UbbParse := OutputStr;
end; { func. UBBParse }


