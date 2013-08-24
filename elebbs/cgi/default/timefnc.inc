(*
**
** Timing routines - check how long a function is running
**
** Created: 26-Aug-2002
** Last update: 26-Aug-2002
**
** (c) 2002 by Maarten Bekers
**
**
*)

procedure timing_Initialize(var timing_StartTime: Real);
begin
  timing_StartTime := GetMicroTimer;
end; { proc. timing_Initialize }

procedure timing_Done(var timing_Finished: Real);
begin
  timing_Finished := GetMicroTimer;
end; { proc. timing_Finished }

function timing_ShowMsecs(Diff: Real): String;
begin
  timing_ShowMSecs := Real2Str(Diff,4,3);
end; { func. timing_ShowMSecs }


function timing_ShowDifference(timing_StartTime, timing_Finished: Real): String;
var TmpS: String;
begin
  TmpS := timing_ShowMSecs(timing_Finished - timing_StartTime);

  timing_ShowDifference :=
    Real2Str(timing_Finished - timing_StartTime, 4, 3) + ' seconds';
end; { func. timing_ShowDifference }

procedure timing_ShowTimeFooter(timing_StartTime, timing_Finished: Real);
begin
  {-- and display the total parse time ---------------------------------------}
  WriteLn('<BR><BR><CENTER><font color="white" size="1">Processing time:',
          timing_ShowDifference(timing_StartTime, timing_Finished),
          '.</FONT></CENTER>');
end; { proc. timing_ShowTimeFooter }

