Unit MKMsgOpn;
{$I MKB.Def}

Interface

Uses MKMsgAbs;

Function OpenMsgArea(Var Msg: AbsMsgPtr; Area: Word): Boolean;
Function CloseMsgArea(Var Msg: AbsMsgPtr): Boolean;

Implementation


Uses MKOpen, MKCfgAbs;

Function OpenMsgArea(Var Msg: AbsMsgPtr; Area: Word): Boolean;
  Begin
  OpenMsgArea := OpenOrCreateMsgArea(Msg, Config^.GetMsgAreaId(Area));
  End;


Function CloseMsgArea(Var Msg: AbsMsgPtr): Boolean;
  Begin
  If Msg <> Nil Then
    Begin
    CloseMsgArea := (Msg^.CloseMsgBase = 0);
    Dispose(Msg, Done);
    End
  Else
    CloseMsgArea := False;
  End;

End.
