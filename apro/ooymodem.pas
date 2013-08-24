{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                   OOYMODEM.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OOYmodem;
  {-Provides Ymodem/YmodemG recieve and transmit functions (using OOP)}

interface

uses
  {$IFNDEF DELPHI}
    Dos,
  {$ENDIF}
  GenDos,
  {$IFDEF UseOPro}
  OpInline,
  OpString,
  OpRoot,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpInline,
  TpString,
  {$ENDIF}
  ApMisc,
  ApPort,
  ApTimer,
  OoCom,
  OoAbsPcl,
  OoXmodem,
  CfgRec;

type
  {Ymodem protocol transmit states}
  YmodemStateType = (
    {Transmit states}
    tyInitial,              {0  Get next file}
    tyHandshake,            {1  Waiting for handshake}
    tyGetFileName,          {2  Get the next file to transmit}
    tySendFileName,         {3  Format and send file name block}
    tyDraining,             {4  Waiting for protocol block to drain}
    tyReplyPending,         {5  Waiting for reply to name block}
    tyPrepXmodem,           {6  Prepare to enter Xmodem state table}
    tySendXmodem,           {7  Calling Xmodem state table}
    tyFinished,             {8  Send EOT}
    tyFinishDrain,          {9  Wait for last block to go out}
    tyFinishAck,            {10 Wait for last block ACK}
    tyDone,                 {11  Signal end of protocol}

    {Receive states}
    ryInitial,              {12 Initialize vars, get buffers, etc.}
    ryDelay,                {13 Delay the handshake for Telix}
    ryWaitForHSReply,       {14 Waiting for 1st reply to handshake}
    ryWaitForBlockStart,    {15 Wait for block start}
    ryCollectBlock,         {16 Collect received chars into DataBlock}
    ryProcessBlock,         {17 Process complete DataBlock}
    ryOpenFile,             {18 Extract file info}
    ryPrepXmodem,           {19 Prepare to enter Xmodem state}
    ryReceiveXmodem,        {20 Calling Xmodem state table}
    ryFinished,             {21 Clean up}
    ryDone);                {22 Signal end of protocol}

  {A Ymodem/YmodemG protocol object}
  YmodemProtocolPtr = ^YmodemProtocol;
  YmodemProtocol = object(XmodemProtocol)
    YmodemState    : YmodemStateType;   {Current Ymodem state}
    SaveName       : PathStr;           {Saved file name}
    SaveLen        : LongInt;           {Saved file length}
    FilesSent      : Boolean;           {True if we actually sent a file}
    FileHeader     : ^DataBlockType;    {Needed for file name block}
    NewDT          : LongInt;           {New date/time stampt}

    constructor Init(APPtr : AbstractPortPtr; Use1K, UseGMode : Boolean);
      {-Allocates and initializes a protocol control block}
    constructor InitCustom(APPtr : AbstractPortPtr;
                           Use1K, UseGMode : Boolean;
                           Options : Word);
      {-Allocates and initializes a protocol control block with options}
    destructor Done; virtual;
      {-Destroy Ymodem object}
    procedure PrepareTransmitPart; virtual;
      {-Prepare to transmit Ymodem batch}
    function ProtocolTransmitPart : ProtocolStateType; virtual;
      {-Perform one increment of a Ymodem transmit}
    procedure PrepareReceivePart; virtual;
      {-Prepare to receive Ymodem batch}
    function ProtocolReceivePart : ProtocolStateType; virtual;
      {-Perform one increment of a Ymodem receive}

    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load an XmodemProtocol object from a stream}
    {$ENDIF}

    {#Z+}
    {++++ Internal methods ++++}
    function apGetFirstBlockNum : Byte; virtual;
    {#Z-}
  end;

  {$IFDEF UseStreams}
  procedure YmodemProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing ymodem objects}
  {$ENDIF}

implementation

  constructor YmodemProtocol.Init(APPtr : AbstractPortPtr;
                                  Use1K, UseGMode : Boolean);
    {-Allocates and initializes a protocol control block}
  begin
    if not YmodemProtocol.InitCustom(APPtr,
                                     Use1K, UseGMode,
                                     DefProtocolOptions) then
      Fail;
  end;

  constructor YmodemProtocol.InitCustom(APPtr : AbstractPortPtr;
                                        Use1K, UseGMode : Boolean;
                                        Options : Word);
    {-Allocates and initializes a protocol control block with options}
  begin
    {Initialize ancestor}
    XmodemProtocol.InitCustom(APPtr, Use1K, UseGMode, Options);

    {Allocate the name block buffer}
    if not GetMemCheck(FileHeader, SizeOf(FileHeader^)+5) then begin
      APPtr^.GotError(epFatal+ecOutOfMemory);
      Done;
      Fail;
    end;

    {Finish up}
    OneKMode := Use1K;
    GMode := UseGMode;
    BatchProtocol := True;
    if GMode then begin
      ProtType := YmodemG;
      TurnDelay := 0;
    end else
      ProtType := Ymodem;

    {Don't ask for any EOT retries}
    EotCheckCount := 0;

    {Set write fail option to rename}
    WriteFailOpt := WriteRename;
  end;

  destructor YmodemProtocol.Done;
    {-Destroy Ymodem object}
  begin
    FreeMemCheck(FileHeader, SizeOf(FileHeader^)+5);
    XmodemProtocol.Done;
  end;

  function YmodemProtocol.apGetFirstBlockNum : Byte;
      {-Returns the first block numbers (0 for header, 1 for file)}
  begin
    apGetFirstBlockNum := FirstBlockNum;
  end;

  procedure YmodemProtocol.PrepareTransmitPart;
    {-Prepare to transmit a Ymodem batch}
  begin
    AbstractProtocol.PrepareTransmitPart;
    FilesSent := False;

    {Reset status vars}
    apResetStatus;

    {Show first status}
    AsyncStatus := ecHandshakeInProgress;
    apShowFirstStatus;
    AsyncStatus := ecOk;

    {Set first state}
    YmodemState := tyInitial;
  end;

  function YmodemProtocol.ProtocolTransmitPart : ProtocolStateType;
    {-Perform one increment of Ymodem batch transmit}
  var
    S : String;
    Len : Byte;
    I : Integer;
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    C : Char;
    SaveState : ProtocolStateType;

    function CheckErrors : Boolean;
      {-Increment block errors, return True if too many}
    begin
      Inc(BlockErrors);
      Inc(TotalErrors);
      CheckErrors := BlockErrors > MaxBlockErrors;
    end;

  begin
    {General stuff - only do if not dropping through to Xmodem state machine}
    if YmodemState <> tySendXmodem then begin

      {Check for user abort}
      if (YmodemState <> tyFinishDrain) and                            {!!.01}
         (YmodemState <> tyFinished) and                               {!!.01}
         apHandleAbort then begin                                      {!!.01}
        YmodemState := tyFinished;
        {Need to log cancellation here since APXMODEM won't see it}
        LogFile(@Self, lfTransmitFail);
      end;

      {Show status periodically}
      if TimerExpired(StatusTimer) or ForceStatus then begin
        ForceStatus := False;
        NewTimer(StatusTimer, StatusInterval);
        apUserStatus(False, False);
      end;
    end;

    {Process current state}
    case YmodemState of
      tyInitial :
        begin
          {Check for handshake character}
          YmodemState := tyHandshake;
          HandshakeAttempt := 0;
          if not apPrepHandshake then
            YmodemState := tyFinished;
        end;

      tyHandshake :
        begin
          if APort^.CharReady then
            if apProcessHandshake then begin
              {Start protocol timer now}
              NewTimer(Timer, 1);
              BlockErrors := 0;
              YmodemState := tyGetFileName;
              {If GMode don't allow any more errors}
              if GMode then
                MaxBlockErrors := 0;
            end else begin
              if AsyncStatus = ecInitCancel then
                YmodemState := tyFinished
              else if not apPrepHandshake then
                YmodemState := tyFinished
            end
          else
            if TimerExpired(ReplyTimer) then
              if not apPrepHandshake then
                YmodemState := tyFinished;
        end;

      tyGetFileName :
        begin
          if NextFile(@Self, SaveName) then begin
            {Format a file name block}
            Pathname := StUpcase(SaveName);

            {Make a Ymodem file header record}
            FillChar(FileHeader^, SizeOf(FileHeader^), 0);

            {Fill in the file name}
            FSplit(SaveName, Dir, Name, Ext);
            if FlagIsSet(apFlags, apIncludeDirectory) then
              S := SaveName
            else
              S := Name + Ext;

            {Change name to lower case, change '\' to '/'}
            Len := Length(S);
            for I := 1 to Len do begin
              S[I] := LoCaseMac(S[I]);
              if S[I] = '\' then
                S[I] := '/';
            end;
            Move(S[1], FileHeader^, Len);

            {Open file now to get size and date stamp}
            apPrepareReading;

            {Continue only if the file was opened ok}
            if AsyncStatus = ecOk then begin
              {Save the file length}
              SaveLen := SrcFileLen;

              {Fill in file size}
              Str(SrcFileLen, S);
              Move(S[1], FileHeader^[Len+2], Length(S));
              Inc(Len, Length(S));

              {Convert time stamp to Ymodem format and stuff in FileHeader}
              if SrcFileDate <> 0 then begin
                S := ' ' + apOctalStr(apPackToYMTimeStamp(SrcFileDate));
                Move(S[1], FileHeader^[Len+2], Length(S));
                Len := Len + 2 + Length(S);
              end;

              {Determine block size from the used part of the FileHeader}
              if Len <= 128 then begin
                BlockLen := 128;
                OneKMode := False;
                StartChar := cSoh;
              end else begin
                BlockLen := 1024;
                OneKMode := True;
                StartChar := cStx;
              end;

              {Init status vars for the header transfer}
              SrcFileLen := BlockLen;
              BytesRemaining := BlockLen;
              BytesTransferred := 0;
              ElapsedTics := 0;
              Pathname := '';

              {Go send the file header}
              YmodemState := tySendFileName;
              CharsLeft := 0;
              OutBufPos := 1;
            end else begin
              APort^.GotError(epFatal+AsyncStatus);
              apShowLastStatus;
              YmodemState := tyDone;
              APort^.PR^.ProtocolActive := False;
            end;
          end else
            YmodemState := tyFinished;
        end;

      tySendFileName :
        begin
          {Send the file header}
          BlockNum := 0;
          apTransmitBlock(FileHeader^, BlockLen, ' ');
          if AsyncStatus <> ecOk then begin
            YmodemState := tyFinished;
            ProtocolTransmitPart := psReady;
            Exit;
          end;

          {If we get this far we will eventually need a cleanup block}
          FilesSent := True;

          {Wait for the buffer to drain}
          YmodemState := tyDraining;
          NewTimer(ReplyTimer, DrainWait);
        end;

      tyDraining :
        begin
          if (APort^.OutBuffUsed <= 1) or TimerExpired(ReplyTimer) then begin
            NewTimer(ReplyTimer, BlockWait);
            YmodemState := tyReplyPending;
          end;
        end;

      tyReplyPending :
        begin
          if APort^.CharReady then begin
            if GMode then
              YModemState := tyPrepXmodem
            else if apProcessBlockReply then
              YmodemState := tyPrepXmodem
            else if CheckErrors then
              YmodemState := tyFinished
            else
              YmodemState := tySendFilename;
          end else
            if TimerExpired(ReplyTimer) then
              if CheckErrors then
                YmodemState := tyFinished
              else
                YmodemState := tySendFilename;
        end;

      tyPrepXmodem :
        begin
          {Reset some status vars}
          BytesTransferred := 0;
          ElapsedTics := 0;
          Inc(InProgress);

          {Restore the pathname and file size}
          Pathname := StUpcase(SaveName);
          SrcFileLen := SaveLen;
          BytesRemaining := SaveLen;

          {Start transmitting the file with 1K blocks}
          OneKMode := True;
          BlockLen := 1024;
          StartChar := cStx;
          FirstBlockNum := 1;

          CheckType := bcChecksum1;
          ForceStatus := True;
          XmodemState := txInitial;
          YmodemState := tySendXmodem;
          SaveState := psReady;
          {DataBlock := nil;}                                          {!!.01}
        end;

      tySendXmodem :
        begin
          {Let the Xmodem state machine handle it}
          SaveState := XmodemProtocol.ProtocolTransmitPart;
          if SaveState = psFinished then begin
            if AsyncStatus = ecOk then
              YmodemState := tyInitial
             else
              YmodemState := tyFinished;

            {Say we're still in the protocol}
            APort^.PR^.ProtocolActive := True;
          end;
        end;

      tyFinished :
        begin
          YmodemState := tyFinishDrain;
          apFinishReading;
          if (AsyncStatus = ecUserAbort) or
             (AsyncStatus = ecCancelRequested) then begin
            apShowLastStatus;
            YmodemState := tyDone;
            APort^.PR^.ProtocolActive := False;
			LogFile(@Self, lfTransmitFail);
          end;

          if FilesSent and (YmodemState <> tyDone) then begin          {!!.01}
            {Send an empty header block to indicate end of Batch}
            FillChar(FileHeader^, 128, 0);
            BlockNum := 0;
            OneKMode := False;
            BlockLen := 128;
            StartChar := cSoh;
            CharsLeft := 0;
            OutBufPos := 1;
            apTransmitBlock(FileHeader^, BlockLen, ' ');
          end;
          NewTimer(ReplyTimer, FinishWait);
        end;

      tyFinishDrain :
        if (APort^.OutBuffUsed <= 1) or TimerExpired(ReplyTimer) then begin
          {Wait for Ack}
          YmodemState := tyFinishAck;
          NewTimer(ReplyTimer, BlockWait);
        end;

      tyFinishAck :
        if APort^.CharReady or TimerExpired(ReplyTimer) then begin
          {Get and discard Ack, or whatever}
          if APort^.CharReady then
            APort^.GetChar(C);
          YmodemState := tyDone;
          apShowLastStatus;
          APort^.PR^.ProtocolActive := False;
        end;
    end;

    {Set function result}
    case YmodemState of
      tySendXmodem       : ProtocolTransmitPart := SaveState;

      tyInitial,
      tyGetFileName,
      tySendFileName,
      tyPrepXmodem,
      tyFinished         : ProtocolTransmitPart := psReady;

      tyFinishAck,
      tyFinishDrain,
      tyHandshake,
      tyDraining,
      tyReplyPending     : ProtocolTransmitPart := psWaiting;

      tyDone             : begin
							 ProtocolTransmitPart := psFinished;
							 LogFile(@Self, lfTransmitFail)
						   end; { tyDone }
    end;
  end;

  procedure YmodemProtocol.PrepareReceivePart;
    {-Perform one increment of a Ymodem receive}
  begin
    XmodemProtocol.PrepareReceivePart;
    {Reset status vars}
    apResetStatus;
    apShowFirstStatus;
    YmodemState := ryInitial;
  end;

  function YmodemProtocol.ProtocolReceivePart : ProtocolStateType;
    {-Ymodem receive state machine}
  label
    ExitPoint;
  var
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    C : Char;
    F : File;
    S : String;
    SLen : Byte absolute S;
    CurSize : LongInt;
    BlockSize : Word;
    BlockPos, I : Integer;
    Finished : Boolean;
    Code : NumReadType;
    ResultTmp : Word;
    SaveState : ProtocolStateType;
    RcvStatus : Word;
    {Cnt : Word;}                                                      {!!.03}

    function CheckErrors : Boolean;
      {-Increment block errors, return True if too many}
    begin
      Inc(BlockErrors);
      Inc(TotalErrors);
      CheckErrors := BlockErrors > MaxBlockErrors;
    end;

  begin
    {General stuff - only do if not dropping through to Xmodem state machine}
    if YmodemState <> ryReceiveXmodem then begin

      {Check for user abort}
      if apHandleAbort then
        YmodemState := ryFinished;

      {Show status periodically}
      if TimerExpired(StatusTimer) or ForceStatus then begin
        ForceStatus := False;
        NewTimer(StatusTimer, StatusInterval);
        if InProgress <> 0 then
          apUserStatus(False, False);
      end;
    end;

    {Process current state}
    case YmodemState of
      ryInitial :
        begin
          {Manually reset status vars before getting a file header}
          SrcFileLen := 0;
          BytesRemaining := 0;
          BytesTransferred := 0;
          ElapsedTics := 0;
          BlockNum := 0;
          Pathname := '';

          {Get a ymodem header block (really a 1 block xmodem transfer)}
          FillChar(FileHeader^[1], SizeOf(FileHeader^), 0);
          OverheadLen := 4;
          CheckType := bcCrc16;
          OneKMode := False;
          BlockSize := 128;
          BlockNum := 0;

          {Testing shows a short delay is required here for Telix}
          NewTimer(ReplyTimer, TelixDelay);
          YmodemState := ryDelay;
        end;

      ryDelay :
        if TimerExpired(ReplyTimer) then begin
          {Finished with Telix delay, send handshake}
          HandshakeChar := apGetHandshakeChar;
          APort^.PutChar(HandshakeChar);
          EotCounter := 0;
          CanCounter := 0;

          {Start waiting for handshake reply}
          YmodemState := ryWaitForHSReply;
          NewTimer(ReplyTimer, HandshakeWait);
          TimerPending := True;
        end;

      ryWaitForHSReply :
        begin
          if APort^.CharReady then begin
            YmodemState := ryWaitForBlockStart;
          end else
            if TimerExpired(ReplyTimer) then
              if CheckErrors then
                YmodemState := ryFinished
              else begin
                if BlockErrors > 3 then begin
                  CheckType := bcChecksum1;
                  OverheadLen := 3;
                  HandshakeChar := cNak;
                end;
                APort^.PutChar(HandshakeChar);
                NewTimer(ReplyTimer, HandshakeWait);
              end;
        end;

      ryWaitForBlockStart :
        begin
          if APort^.CharReady then begin
            if apCheckForBlockStart(C) then begin
              case C of
                cSoh,
                cStx :
                  begin
                    if C = cSoh then
                      BlockLen := 128
                    else
                      BlockLen := 1024;
                    if TimerPending then
                      NewTimer(Timer, 1);
                    YmodemState := ryCollectBlock;
                    BlkIndex := 0;
                    if GMode then
                      MaxBlockErrors := 0;
                  end;
                cCan :
                  begin
                    EotCounter := 0;
                    Inc(CanCounter);
                    if CanCounter > 2 then begin
                      apCancel;
                      YmodemState := ryFinished;
                    end;
                  end;
                cEot :
                  begin
                    CanCounter := 0;
                    Inc(EotCounter);
                    if EotCounter = 1 then
                      APort^.PutChar(cNak)
                    else begin
                      APort^.PutChar(cAck);
                      YmodemState := ryFinished;
                    end;
                  end;
              end;
            end;
          end else begin
            {No chars yet, check timeout}
            if TimerExpired(ReplyTimer) then
              if CheckErrors then
                YmodemState := ryFinished
              else begin
                APort^.FlushInBuffer;
                YmodemState := ryDelay;
              end;
          end;
        end;

      ryCollectBlock :
        if APort^.CharReady then begin
          {Cnt := 1;}                                                  {!!.03}
          while APort^.CharReady and
                {(Cnt < 10) and}                                       {!!.03}
                (BlkIndex < BlockLen + OverheadLen) do begin
            APort^.GetChar(C);
            Inc(BlkIndex);
            {Inc(Cnt);}                                                {!!.03}
            FileHeader^[BlkIndex] := C;
          end;

          if BlkIndex >= BlockLen + OverheadLen then
            {Got a complete block, go process it}
            YmodemState := ryProcessBlock
          else if TimerExpired(ReplyTimer) then
            if CheckErrors then
              YmodemState := ryFinished
            else
              {Timeout out waiting for initial block, resend handshake}
              YmodemState := ryInitial;
        end;

      ryProcessBlock :
        begin
          {Go process data already in DataBlock}
          apReceiveBlock(FileHeader^, BlockSize, HandshakeChar);
          RcvStatus := AsyncStatus;
          apSendHandshakeChar(HandshakeChar);

          {Extract file info if we got block ok}
          if RcvStatus = ecOk then begin
            {Finished if entire block is null}
            Finished := True;
            I := 1;
            while (I < 120) and Finished do begin                      {!!.03}
              if FileHeader^[I] <> #0 then
                Finished := False;
              Inc(I);
            end;

            {If finished, send last ack and exit}
            if Finished then begin
              YmodemState := ryFinished;
              goto ExitPoint;
            end;

            {Extract the file name from the header}
            BlockPos := 1;
            I := 0;
            while (FileHeader^[BlockPos] <> #0) and (BlockPos < 255) do begin
              Inc(I);
              S[I] := FileHeader^[BlockPos];
              Inc(BlockPos);
            end;
            SLen := I;

            {Change all '/' to '\'. Change name to all upper case}
            for I := 1 to SLen do begin
              if S[I] = '/' then
                S[I] := '\';
              S[I] := Upcase(S[I]);
            end;
            Pathname := S;

            {Check the existance of the directory and file name}
            FSplit(Pathname, Dir, Name, Ext);

            {Should we use its directory or ours?}
            if not FlagIsSet(apFlags, apHonorDirectory) then
              PathName := AddBackSlash(DestDir) + Name + Ext;

            {Extract the file size}
            I := 1;
            Inc(BlockPos);
            while (FileHeader^[BlockPos] <> #0) and
                  (FileHeader^[BlockPos] <> ' ') and
                  (I <= 255) do begin
              S[I] := FileHeader^[BlockPos];
              Inc(I);
              Inc(BlockPos);
            end;
            Dec(I);
            SLen := I;
            if SLen = 0 then
              SrcFileLen := 0
            else begin
              Val(S, SrcFileLen, Code);
              if Code <> 0 then
                SrcFileLen := 0;
            end;
            BytesRemaining := SrcFileLen;

            {Extract the file date/time stamp}
            I := 1;
            Inc(BlockPos);
            while (FileHeader^[BlockPos] <> #0) and
                  (FileHeader^[BlockPos] <> ' ') and
                  (I <= 255) do begin
              S[I] := FileHeader^[BlockPos];
              Inc(I);
              Inc(BlockPos);
            end;
            Dec(I);
            SLen := I;
            if SLen = 0 then
              NewDT := 0
            else begin
              NewDT := apOctalStr2Long(S);
              if NewDT = 0 then begin
                {Invalid char in date/time stampt, show the error and continue}
                APort^.GotError(epNonFatal+ecInvalidDateTime);
                NewDT := 0;
                apUserStatus(False, False);
                AsyncStatus := ecOk;
              end;
            end;

            {Manually reset status vars before getting file}
            BytesTransferred := 0;
            ElapsedTics := 0;

            {Receive the file using CRC and 1K blocks}
            CheckType := bcCrc16;
            OneKMode := True;
            BlockLen := 1024;
            FirstBlockNum := 1;
            SaveLen := SrcFileLen;

            {Go prep Xmodem}
            YmodemState := ryPrepXmodem;
          end else
            {Error getting name block...}
            if GMode then
              {Can't recover when in GMode, go quit}
              YmodemState := ryFinished
            else begin
              {Nak already sent, go get block again}
              YmodemState := ryWaitForHSReply;
              NewTimer(ReplyTimer, HandshakeWait);
            end;
        end;

      ryPrepXmodem :
        begin
          XmodemProtocol.PrepareReceivePart;
          YmodemState := ryReceiveXmodem;
          SaveState := psReady;
        end;

      ryReceiveXmodem :
        begin
          SaveState := XmodemProtocol.ProtocolReceivePart;

          if SaveState = psFinished then begin
            if AsyncStatus = ecOk then begin
              {If this is a file, check for truncation and file date}
              {SaveMode := FileMode;}                           {!!.02}{!!.03}
              {FileMode := $40;}                                {!!.02}{!!.02}
              Assign(F, Pathname);
              {$I-}
              Reset(F, 1);
              {FileMode := SaveMode;}                           {!!.02}{!!.03}
              if IOResult = 0 then begin
                {If a new file size was supplied, truncate to that length}
                if SaveLen <> 0 then begin

                  {Get the file size of the file (as received)}
                  CurSize := FileSize(F);

                  {If the requested file size is within one block, truncate the file}
                  if (CurSize - SaveLen) < 1024 then begin
                    Seek(F, SaveLen);
                    Truncate(F);
                    ResultTmp := IOResult;
                    if ResultTmp <> 0 then begin
                      APort^.GotError(epNonFatal+ResultTmp);
                      AsyncStatus := ecOk;
                    end;
                  end;
                end;

                {If a new date/time stamp was specified, update the file time}
                if NewDT <> 0 then begin
                  NewDT := apYMTimeStampToPack(NewDT);
                  SetFTime(F, NewDT);
                  ResultTmp := IOResult;
                  if ResultTmp <> 0 then begin
                    APort^.GotError(epNonFatal+ResultTmp);
                    AsyncStatus := ecOk;
                  end;
                end;
              end;
              Close(F);
              if IOResult <> 0 then ;

              {Go look for another file}
              YmodemState := ryInitial;
              NewTimer(ReplyTimer, HandshakeWait);
              ForceStatus := True;
            end else
              YmodemState := ryFinished;
          end;
          APort^.PR^.ProtocolActive := True;
        end;

      ryFinished :
        begin
          apShowLastStatus;
          YmodemState := ryDone;
          APort^.PR^.ProtocolActive := False;
        end;
    end;

ExitPoint:
    {Set function result}
    case YmodemState of
      ryReceiveXmodem     : ProtocolReceivePart := SaveState;

      ryInitial,
      ryOpenFile,
      ryProcessBlock,
      ryFinished,
      ryPrepXmodem        : ProtocolReceivePart := psReady;

      ryDelay,
      ryWaitForHSReply,
      ryWaitForBlockStart,
      ryCollectBlock      : ProtocolReceivePart := psWaiting;

      ryDone              : ProtocolReceivePart := psFinished;
    end;
  end;

  {$IFDEF UseStreams}
  constructor YmodemProtocol.Load(var S : IdStream);
    {-Load a modemProtocol object from a stream}
  begin
    FileHeader := nil;

    {Load the parent}
    if not XModemProtocol.Load(S) then begin
      Done;
      Fail;
    end;

    {Check for errors}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Allocate the name block buffer}
    if not GetMemCheck(FileHeader, SizeOf(FileHeader^)+5) then begin   {!!.01}
      AsyncStatus := ecOutOfMemory;
      Done;
      Fail;
    end;
  end;

  procedure YmodemProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing ymodem objects}
  begin
    XmodemProtocolStream(SPtr);
    SPtr^.RegisterType(otYmodemProtocol, veYmodemProtocol,
                       TypeOf(YmodemProtocol),
                       @YmodemProtocol.Store, @YmodemProtocol.Load);
    SPtr^.RegisterOldVersion(otYmodemProtocol, 00,
                       TypeOf(YmodemProtocol),
                       @YmodemProtocol.Load00);
  end;
  {$ENDIF}

end.
