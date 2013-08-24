Unit MKOpen; {Open a message area using an MsgAreaId}

  {$I compiler.inc}
  {$I MKB.Def}

{
     MKOpen - Copyright 1993 by Mark May - MK Software
     You are free to use this code in your programs, however
     it may not be included in Source/TPU function libraries
     without my permission.

     Mythical Kingom Tech BBS (513)237-7737 HST/v32
     FidoNet: 1:110/290
     Rime: ->MYTHKING
     You may also reach me at maym@dmapub.dma.org
}


Interface

Uses MKMsgAbs;

Function OpenMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
Function OpenOrCreateMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
Function CloseMsgArea(Var Msg: AbsMsgPtr): Boolean;
Function InitMsgPtr(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
Function DoneMsgPtr(Var Msg: AbsMsgPtr): Boolean;

Implementation


Uses {$IFDEF WITH_HUDSON}
       MKMsgHud
     {$ENDIF}
     {$IFDEF WITH_FIDO}
      ,MKMsgFid
     {$ENDIF}
     {$IFDEF WITH_SQUISH}
       ,MKMsgSqu
     {$ENDIF}
     {$IFDEF WITH_EZYCOM}
        ,MKMsgEzy
     {$ENDIF}
     {$IFDEF WITH_JAM}
         ,MKMsgJam
     {$ENDIF};

{ Area ids begin with identifier for msg base type }
{ The following characters are already reserved    }
{   B = PC-Board            }
{   E = Ezycomm             }
{   F = Fido *.Msg          }
{   H = Hudson              }
{   I = ISR - msg fossil    }
{   J = JAM                 }
{   M = MK-Merlin           }
{   P = *.PKT               }
{   Q = QWK/REP             }
{   R = Renegade            }
{   S = Squish              }
{   W = Wildcat             }


Function OpenMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
  Begin
  If InitMsgPtr(Msg, MsgAreaId) Then
    Begin

    OpenMsgArea := True;

    If Msg^.OpenMsgBase <> 0 Then
      Begin
      OpenMsgArea := False;
      If DoneMsgPtr(Msg) Then;
      End
    End
  Else
    OpenMsgArea := False;
   if IoResult > 00 then ;
  End;


Function OpenOrCreateMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
  Begin
  If InitMsgPtr(Msg, MsgAreaId) Then
    Begin

    OpenOrCreateMsgArea := True;
    If Not Msg^.MsgBaseExists Then
      If Not Msg^.CreateMsgBase(200, 10) = 0 Then
        OpenOrCreateMsgArea := False;

    If Msg^.OpenMsgBase <> 0 Then
      Begin

      OpenOrCreateMsgArea := False;
      if Msg^.CloseMsgBase <> 0 then;

      If DoneMsgPtr(Msg) Then;

      if IoResult > 00 then ;
      End;

    End;

   if IoResult > 00 then ;
  End;


Function CloseMsgArea(Var Msg: AbsMsgPtr): Boolean;
  Begin
  If Msg <> Nil Then
    Begin
    CloseMsgArea := (Msg^.CloseMsgBase = 0);
    If DoneMsgPtr(Msg) Then;
    End
  Else
    CloseMsgArea := False;
  End;


Function InitMsgPtr(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
  Begin

  Msg := Nil;
  InitMsgPtr := True;
  Case UpCase(MsgAreaId[1]) of
{$IFDEF WITH_HUDSON} 'H': Msg := New(HudsonMsgPtr, Init);   {$ENDIF}
{$IFDEF WITH_SQUISH} 'S': Msg := New(SqMsgPtr, Init);       {$ENDIF}
{$IFDEF WITH_FIDP}   'F': Msg := New(FidoMsgPtr, Init);     {$ENDIF}
{$IFDEF WITH_EZYCOM} 'E': Msg := New(EzyMsgPtr, Init);      {$ENDIF}
{$IFDEF WITH_JAM}    'J': Msg := New(JamMsgPtr, Init);      {$ENDIF}
    Else
      InitMsgPtr := False;
    End;


  If Msg <> Nil Then
    Msg^.SetMsgPath(Copy(MsgAreaId, 2, 128));


  if Msg=nil then InitMsgPtr := False;

  End;


Function DoneMsgPtr(Var Msg: AbsMsgPtr): Boolean;
  Begin

  DoneMsgPtr := true;
  If Msg <> Nil Then
    Dispose(Msg, Done);

  Msg := Nil;
  End;

End.
