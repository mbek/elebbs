
procedure SetLoginUnixTimeStamp;
var TmpDate    : String;
    TmpTime    : String;

    TempYear,
    TempMonth,
    TempDay,
    TempHour,
    TempMins   : Integer;
    DateFmt    : Integer;
begin
  {-- get last login time/date ----------------------------------------------}
  DateFmt := FVal(EleCodeStr('|F<'));
  TmpDate := EleCodeStr('|FF');
  TmpTime := EleCodeStr('|FG');

  {-- Fix the date ----------------------------------------------------------}
  Case DateFmt of
   { DD-MM-YY    }  01 : begin
                           TempYear := FVal(Copy(TmpDate, 7, 2));
                           TempMonth:= FVal(Copy(TmpDate, 4, 2));
                           TempDay  := FVal(Copy(TmpDate, 1, 2));
                         end; { MM-DD-YY }
   { MM-DD-YY    }  02 : begin
                           TempYear := FVal(Copy(TmpDate, 7, 2));
                           TempMonth:= FVal(Copy(TmpDate, 1, 2));
                           TempDay  := FVal(Copy(TmpDate, 4, 2));
                         end; { DD-MM-YY }
   { YY-MM-DD    }  03 : begin
                           TempYear := FVal(Copy(TmpDate, 1, 2));
                           TempMonth:= FVal(Copy(TmpDate, 4, 2));
                           TempDay  := FVal(Copy(TmpDate, 7, 2));
                         end; { YY-MM-DD }
   { DD-Mmm-YY   }  04 : begin
                           TempYear := FVal(Copy(TmpDate, 8, 2));
                           TempMonth := MonthNameToNum(Copy(TmpDate, 4, 3));
                           TempDay := FVal(Copy(TmpDate, 1, 2));
                          end; { DD-Mmmm-YY }
   { DD-MM-YYYY  }  05 : begin
                           TempYear := FVal(Copy(TmpDate, 7, 4));
                           TempMonth:= FVal(Copy(TmpDate, 4, 2));
                           TempDay  := FVal(Copy(TmpDate, 1, 2));
                         end; { DD-MM-YYYY }
   { MM-DD-YYYY  }  06 : begin
                           TempYear := FVal(Copy(TmpDate, 7, 4));
                           TempMonth:= FVal(Copy(TmpDate, 1, 2));
                           TempDay  := FVal(Copy(TmpDate, 4, 2));
                         end; { MM-DD-YYYY}
   { YYYY-MM-DD  }  07 : begin
                           TempYear := FVal(Copy(TmpDate, 3, 2));
                           TempMonth:= FVal(Copy(TmpDate, 6, 2));
                           TempDay  := FVal(Copy(TmpDate, 7, 4));
                         end; { YYYY-MM-DD }
   { DD-Mmm-YYYY }  08 : begin
                           TempYear := FVal(Copy(TmpDate, 8, 4));
                           TempMonth := MonthNameToNum(Copy(TmpDate, 4, 3));
                           TempDay := FVal(Copy(TmpDate, 1, 2));
                         end; { DD-Mmm-YYYY }
     else ;
  end; { case }

  {-- fix the date if necessary ----------------------------------------------}
  if TempYear < 1900 then
    begin
      if TempYear < 80 then
        TempYear := TempYear + 2000
          else TempYear := TempYear + 1900;
    end; { if }


  {-- First we put it all in seperate variables -----------------------------}
  TempHour := FVal(Copy(TmpTime, 1, 2));
  TempMins := FVal(Copy(TmpTime, 4, 2));

  {-- and now convert it to a unix time stamp -------------------------------}
  LoginUnix := Norm2Unix(TempYear, TempMonth, TempDay, TempHour, TempMins, 0);
end; { proc. SetLoginUnixTimeStamp }


function HasNewMessages(TmpUnix: Longint): Boolean;
begin
  {-- and make the comparison -----------------------------------------------}
  HasNewMessages := (TmpUnix > LoginUnix);
end; { func. HasNewMessages }

function Unix2Str(Date, Fmt: Integer): String;
var Year,
    Month,
    Day,
    Hour,
    Min,
    Sec     : Integer;
begin
  Unix2Norm(Date, Year, Month, Day, Hour, Min, Sec);

  Case Fmt of
    0 : Unix2Str := LeadingZero(Hour, 2) + ':' + LeadingZero(Min, 2);
    1 : Unix2Str := LeadingZero(Hour, 2) + ':' + LeadingZero(Min, 2) + ':' +
                    LeadingZero(Sec, 2);
    2 : Unix2Str := LeadingZero(Day, 2) + '-' + LeadingZero(Month, 2) + '-' +
                    LeadingZero(Year, 4);
    3 : Unix2Str := LeadingZero(Day, 2) + '-' + LeadingZero(Month, 2) + '-' +
                    Copy(FStr(Year), 3, 2);
    4 : Unix2Str := LeadingZero(Month, 2) + '-' + LeadingZero(Day, 2) + '-' +
                    Copy(FStr(Year), 3, 2);
    else Unix2Str := '??';
  end; { case }
end; { func. Unix2Str }

