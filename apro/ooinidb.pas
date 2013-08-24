{$V-,B-,I-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}
{$X+}
{*********************************************************}
{*                   OOINIDB.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1995.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoIniDB;
  {-INI file database management}

interface

uses
{$IFDEF UseTPro}
  TpMemChk,
  TpDos,
  TpString,
{$ENDIF}
{$IFDEF UseOPro}
  OpRoot,
  OpDos,
  OpString,
{$ENDIF}
  ApMisc,
  ApPort,
  OoIni;

const
  MaxDBRecs      = 999;         {Maximum number of database records}
  MaxNameLen     = 21;          {Maximum length of a profile string key}
  MaxIndexLen    = 31;          {Maximum length of an index string}
  NonValue       = '#';         {Value of DB fields SPECIFICALLY left blank}
  DbIndex        = 'Index';     {Item index section heading}
  DbDefaultTitle = 'Defaults';  {Default value section heading}
  dbNumEntries   = '_Entries';  {Number of entries key name}
  DbBogus        = 'None';      {Bogus key name for creating sections}

type
  {Defines a single record}
  IniDataBaseKeyPtr = ^IniDataBaseKey;
  IniDataBaseKey = record
    KeyName  : string;
    DataSize : Word;
    StrType  : Boolean;
    Index    : Boolean;
    Next     : IniDataBaseKeyPtr;
  end;

  {Defines database object}
  IniDataBasePtr = ^IniDataBase;
  IniDataBase = object (IniMgr)
    DictionaryHead : IniDataBaseKeyPtr;
    DictionaryTail : IniDataBaseKeyPtr;
    NumRecords     : Integer;
    RecordSize     : Word;
    DefaultRecord  : Pointer;
    Prepared       : Boolean;

    constructor Init(FName : string; ReadOnly : Boolean; AlwaysRW: Boolean);
      {-Initialize an .INI file database}

    destructor Done; virtual;
      {-Destroy an .INI file database}

    procedure AddIniDBStringField(FieldName : string; MaxLen : Word;
                                 Index     : Boolean);
      {-Add a string field to the .INI file database}

    procedure AddIniDBIntField(FieldName : string);
      {-Add an integer field to the .INI file database}

    procedure PrepareIniDatabase(Defaults : Pointer);
      {-Prepare the databse for reading/writing}

    procedure ChangeIniDefaults(var DefaultRec);
      {-Change the default values for record fields}

    function KeyExists(Key : string) : Boolean;
      {-Return TRUE if an entry with an index of 'Name' exists}

    procedure AddIniRecord(var Rec);
      {-Add a record to the database}

    procedure UpdIniRecord(Key : string; var Rec);
      {-Update a record in the database}

    procedure DelIniRecord(Key : string);
      {-Remove a record from the database}

    procedure GetIniRecord(Key : string; var Rec);
      {-Get a record from the database}

    function NumIniRecs : Integer;
      {-Return the number of records in an INI database}

    procedure WriteToIni(var Rec; Group, IniFile : string);
      {-Write the record to a user-specified .INI file}

    procedure ReadFromIni(var Rec; Group, IniFile : string);
      {-Read the record from a user-specified .INI file}

    {+++internal+++}
    procedure iAddIniKeyPrim(AKeyName  : string;
                             AStrType  : Boolean;
                             AIndex    : Boolean;
                             ADataSize : Word );
    function iIniIndexKey : IniDataBaseKeyPtr;
    function iGetIniDataString(var Rec; Key : IniDataBaseKeyPtr) : string;
    procedure iUpdateIniRecCount;
    procedure iPutIniString(Name, Key, Str : string);
    procedure iRetrieveIniRecord(SecName : string; var Rec; ReadIni : Boolean);
    procedure iSaveIniRecord(SecName : string; var Rec; WriteIni : Boolean);
  end;

implementation

  function Long2Str(L : LongInt) : string;
    {-Convert a long/word/integer/byte/shortint to a string}
  var
    S : string;

  begin
    Str(L, S);
    Long2Str := S;
  end;

  constructor IniDataBase.Init(FName : String; ReadOnly : Boolean;
                               AlwaysRW: Boolean);
    {-Initialize a .INI file database}
  begin
    if not IniMgr.Init(FName,ReadOnly) then
      Fail;
    if ReadOnly = False then
      SetFlushMode(AlwaysRW);
    DictionaryHead := nil;
    DictionaryTail := nil;
    NumRecords     := 0;
    RecordSize     := 0;
    DefaultRecord  := nil;
    Prepared       := False;
  end;

  destructor IniDataBase.Done;
    {-Destroy an .INI file database}
  var
    Temp : IniDataBaseKeyPtr;

  begin
    while (DictionaryHead <> nil) do begin
      Temp := DictionaryHead^.Next;
      FreeMemCheck(DictionaryHead, SizeOf(IniDataBaseKey));
      DictionaryHead := Temp;
    end;
    FreeMemCheck(DefaultRecord, RecordSize);
    IniMgr.Done;
  end;

  procedure IniDataBase.iAddIniKeyPrim(AKeyName  : string;
                                        AStrType  : Boolean;
                                        AIndex    : Boolean;
                                        ADataSize : Word );
    {-Add an .INI key with these attributes to the dictionary}
  var
    NewKey : IniDataBaseKeyPtr;

  begin
      if (LongInt(RecordSize + ADataSize) > $FFF0) then begin
      AsyncStatus := ecDataTooLarge;
      Exit;
    end;

    if AIndex and (Pred(ADataSize) > MaxIndexLen) then begin
      AsyncStatus := ecIndexDataTooLarge;
      Exit;
    end;

    if (Length(AKeyName) > MaxNameLen) then begin
      AsyncStatus := ecKeyTooLong;
      Exit;
    end;

    if not GetMemCheck(NewKey, SizeOf(IniDataBaseKey)) then begin
      AsyncStatus := ecOutOfMemory;
      Exit;
    end;

    NewKey^.KeyName := AKeyName;

    with NewKey^ do begin
      DataSize := ADataSize;
      StrType  := AStrType;
      Index    := AIndex;
      Next     := nil;

      if (DictionaryHead = nil) then begin
        DictionaryHead := NewKey;
        DictionaryTail := NewKey;
      end else begin
        DictionaryTail^.Next := NewKey;
        DictionaryTail       := NewKey;
      end;

      Inc(RecordSize, DataSize);
    end;
{    FreeMemCheck(NewKey, SizeOf(IniDataBaseKey));}
    AsyncStatus := ecOK;
  end;

  procedure IniDataBase.AddIniDBStringField(FieldName : string;
                                             MaxLen    : Word;
                                             Index     : Boolean);
    {-Add a string field to the .INI file database}
  begin
    iAddIniKeyPrim(FieldName, True, Index, MaxLen + 1);
  end;

  procedure IniDataBase.AddIniDBIntField(FieldName : string);
    {-Add an integer field to the .INI file database}
  begin
    iAddIniKeyPrim(FieldName, False, False, SizeOf(Integer));
  end;

  function IniDataBase.iIniIndexKey : IniDataBaseKeyPtr;
    {-Return a pointer to the indexed key}
  var
    CurItem : IniDataBaseKeyPtr;

  begin
    CurItem := DictionaryHead;
    while (CurItem <> nil) do begin
      if CurItem^.Index then begin
        iIniIndexKey := CurItem;
        Exit;
      end;
      CurItem := CurItem^.Next;
    end;
    iIniIndexKey := nil;
  end;

  procedure IniDataBase.PrepareIniDatabase(Defaults : Pointer);
    {-Prepare the databse for reading/writing}
  var
    CurItem : IniDataBaseKeyPtr;
    TempRec : Pointer;
    TempStr : string;

  begin
      {if there are no items defined, it's an error}
      if (DictionaryHead = nil) then begin
        AsyncStatus := ecNoFieldsDefined;
        Exit;
      end;

      if (iIniIndexKey = nil) then begin
        AsyncStatus := ecNoIndexKey;
        Exit;
      end;

      {allocate the default data record}
      if not GetMemCheck(DefaultRecord, RecordSize) then begin
        AsyncStatus := ecOutOfMemory;
        Exit;
      end;
      FillChar(DefaultRecord^, RecordSize, 0);

      {if the .INI file doesn't exist, create a default one}
      if not ExistFile(IniName) then begin
        {create the index section}
        SetProfileString(dbBogus, dbIndex, dbBogus);
        if AsyncStatus <> ecOK then begin
          FreeMemCheck(DefaultRecord, RecordSize);
          Exit;
        end;
        DeleteProfileString(dbBogus, dbIndex);
        if AsyncStatus <> ecOK then begin
          FreeMemCheck(DefaultRecord, RecordSize);
          Exit;
        end;
        {create the defaults section}
        SetProfileString(dbNumEntries, dbDefaultTitle, Long2Str(NumRecords));
        if AsyncStatus <> ecOK then begin
          FreeMemCheck(DefaultRecord, RecordSize);
          Exit;
        end;

        if (Defaults <> nil) then begin
          Prepared := True;
          ChangeIniDefaults(Defaults^);
          if (AsyncStatus <> ecOK) then begin
            Prepared := False;
            FreeMemCheck(DefaultRecord, RecordSize);
            Exit;
          end;
        end;

        NumRecords := 0;
        {since we've created only a default file, we don't need to save it
         if no changes are made after it.}
        NeedUpd := False;

      end else begin
        {load the number of database entries}
        NumRecords := GetProfileInt(dbNumEntries, dbDefaultTitle, 0);

        {load the default record}
        TempRec := DefaultRecord;
        CurItem := DictionaryHead;
        while (CurItem <> nil) do begin
          if not CurItem^.Index then
            if CurItem^.StrType then begin
              TempStr := GetProfileString(CurItem^.KeyName, dbDefaultTitle, '');
                Move(TempStr[0], TempRec^, Length(TempStr) + 1);
            end else
              Integer(TempRec^) := GetProfileInt(CurItem^.KeyName,
                                                 dbDefaultTitle, 0);
          TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
          CurItem := CurItem^.Next;
        end;
      end;

      Prepared := True;

    AsyncStatus := ecOK;
  end;

  procedure IniDataBase.ChangeIniDefaults(var DefaultRec);
    {-Change the default values for record fields}
  var
    CurItem : IniDataBaseKeyPtr;
    TempRec : Pointer;
    TempInt : Integer;
    TempStr : string;

  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    Move(DefaultRec, DefaultRecord^, RecordSize);

    TempRec := DefaultRecord;
    CurItem := DictionaryHead;
    while (CurItem <> nil) do begin
      if not CurItem^.Index then
        if CurItem^.StrType then begin
          Move(TempRec^, TempStr, CurItem^.DataSize);
          SetProfileString(CurItem^.KeyName, dbDefaultTitle, TempStr);
          if AsyncStatus <> ecOK then
            Exit;
        end else begin
          Move(TempRec^, TempInt, CurItem^.DataSize);
          SetProfileString(CurItem^.KeyName, dbDefaultTitle, Long2Str(TempInt));
          if AsyncStatus <> ecOK then
            Exit;
        end;
        TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
        CurItem := CurItem^.Next;
    end;
    AsyncStatus := ecOK;
  end;

  function IniDataBase.KeyExists(Key : string) : Boolean;
    {-Return TRUE if an entry with an index of 'Name' exists}
  var
    Temp : string[5];

  begin
    if not Prepared then begin
      KeyExists := False;
      Exit;
    end;

    Temp := GetProfileString(Key, dbIndex, '');
    KeyExists := (Temp =  NonValue);
  end;

  function IniDataBase.iGetIniDataString(var Rec;
                                          Key : IniDataBaseKeyPtr) : string;
    {-Get a string from an INI data record}
  var
    CurItem : IniDataBaseKeyPtr;
    TempRec : Pointer;
    TempStr : string;

  begin
    CurItem := DictionaryHead;
    TempRec := @Rec;
    while (CurItem <> nil) and (CurItem <> Key) do begin
      TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
      CurItem := CurItem^.Next;
    end;
    if (CurItem = Key) then begin
      Move(TempRec^, TempStr, CurItem^.DataSize);
      iGetIniDataString := TempStr;
    end else
      iGetIniDataString := '';
  end;

  procedure IniDataBase.iUpdateIniRecCount;
    {-Update the NumEntries field in the .INI file}
  var
    Temp : string[5];

  begin
    Temp := Long2Str(NumRecords);
    SetProfileString(dbNumEntries, dbDefaultTitle, Temp);
  end;

  procedure IniDataBase.iPutIniString(Name, Key, Str : string);
    {-Put a string to the .INI file}
  var
    TempStr : string[Length(NonValue)];

  begin
    AsyncStatus := ecOK;
    {if the string is intentionally left blank, exit}
    if (Str = '') then begin
      TempStr := GetProfileString(Key, Name, '');
      if (TempStr = NonValue) then
        Exit;
    end;

    {if the string <> '', write it out}
    if (Str <> '') then
      SetProfileString(Key, Name, Str)
    else
      {if the string = '', delete its database entry}
      DeleteProfileString(Key, Name);
  end;

  procedure IniDataBase.iSaveIniRecord(SecName : string; var Rec; WriteIni : Boolean);
    {-Save an INI record to the database}
  var
    CurItem : IniDataBaseKeyPtr;
    TempRec : Pointer;
    Temp    : string[5];
    TempStr : string;

  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    CurItem := DictionaryHead;
    TempRec := @Rec;
    while (CurItem <> nil) do begin
      if not CurItem^.Index then begin
        if CurItem^.StrType then
          Move(TempRec^, TempStr, CurItem^.DataSize)
        else begin
          Move(TempRec^, TempStr, CurItem^.DataSize);
          TempStr := Long2Str(Integer(TempRec^));
        end;
        if TempStr <> '' then begin
          iPutIniString(SecName, CurItem^.KeyName, TempStr);
          if AsyncStatus <> ecOK then
            Exit;
        end;
      end else
        if WriteIni then begin
          Move(TempRec^, TempStr, CurItem^.DataSize);
          iPutIniString(SecName, CurItem^.KeyName, TempStr);
          if AsyncStatus <> ecOK then
            Exit;
        end;

      TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
      CurItem := CurItem^.Next;
    end;

    AsyncStatus := ecOK;
  end;


  procedure IniDataBase.AddIniRecord(var Rec);
    {-Add a record to the database}
  var
    IndexKey  : IniDataBaseKeyPtr;
    IndexName : string;

  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    if (NumRecords = MaxDBRecs) then begin
      AsyncStatus := ecDatabaseFull;
      Exit;
    end;

    IndexKey  := iIniIndexKey;
    IndexName := iGetIniDataString(Rec, IndexKey);

    if KeyExists(IndexName) then begin
      AsyncStatus := ecRecordExists;
      Exit;
    end;

    {add the entry to the index}
    SetProfileString(IndexName, dbIndex, NonValue);
    if AsyncStatus <> ecOK then
      Exit;

    iSaveIniRecord(IndexName, Rec, False);
    if (AsyncStatus <> ecOK) then
      Exit;

    Inc(NumRecords);
    iUpdateIniRecCount;
  end;

  procedure IniDataBase.UpdIniRecord(Key : string; var Rec);
    {-Update a record in the database}
  var
    IndexKey  : IniDataBaseKeyPtr;
    IndexName : string;

  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    IndexKey  := iIniIndexKey;
    IndexName := iGetIniDataString(Rec, IndexKey);

    if not KeyExists(Key) then begin
      AsyncStatus := ecRecordNotFound;
      Exit;
    end;

    if StUpCase(Key) <> StUpCase(IndexName) then begin
      {if the name has changed, first delete the old entry}
      DelIniRecord(Key);
      if (AsyncStatus <> ecOK) then
        Exit;

      {add a new entry}
      AddIniRecord(Rec);
    end else
      iSaveIniRecord(Key, Rec, False);
  end;

  procedure IniDataBase.DelIniRecord(Key : string);
    {-Remove a record from the database}
  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    if not KeyExists(Key) then begin
      AsyncStatus := ecRecordNotFound;
      Exit;
    end;

    {delete the index entry}
    DeleteProfileString(Key, dbIndex);
    if AsyncStatus <> ecOK then
      Exit;

    {delete the record}
    DeleteProfileGroup(Key);
    if AsyncStatus <> ecOK then
      Exit;

    {update the record count}
    Dec(NumRecords);
    iUpdateIniRecCount;
  end;

  procedure IniDataBase.GetIniRecord(Key : string; var Rec);

  begin
    iRetrieveIniRecord(Key, Rec, False);
  end;

  procedure IniDataBase.iRetrieveIniRecord(SecName : string; var Rec;
                                           ReadIni : Boolean);
    {-Get a record from the database}
  var
    TempRec : Pointer;
    DefRec  : Pointer;
    CurItem : IniDataBaseKeyPtr;
    DefInt  : Integer;
    TempStr : string;
    DefStr  : string;

  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    if not KeyExists(SecName) then begin
      AsyncStatus := ecRecordNotFound;
      Exit;
    end;

    TempRec := @Rec;
    DefRec  := DefaultRecord;

    CurItem := DictionaryHead;
    while (CurItem <> nil) do begin
      if CurItem^.StrType then
        if CurItem^.Index then
          if ReadIni then
            TempStr := GetProfileString(CurItem^.KeyName, SecName, '')
          else
            Move(SecName[0], TempRec^, Length(SecName) + 1)
        else begin
          Move(DefRec^, DefStr, CurItem^.DataSize);
          TempStr := GetProfileString(CurItem^.KeyName, SecName, DefStr);
          Move(TempStr, TempRec^, Length(TempStr) + 1);
          if TempStr = NonValue then
            PChar(TempRec)[0] := #0;
        end
      else begin
        Move(DefRec^, DefInt, CurItem^.DataSize);
        Integer(TempRec^) := GetProfileInt(CurItem^.KeyName, SecName, DefInt);
      end;
      TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
      DefRec  := AddWordToPtr(DefRec, CurItem^.DataSize);
      CurItem := CurItem^.Next;
    end;

    AsyncStatus := ecOK;
  end;

  function IniDataBase.NumIniRecs : Integer;
    {-Return the number of records in an INI database}
  begin
    NumIniRecs := NumRecords;
  end;

  procedure IniDataBase.WriteToIni(var Rec; Group, IniFile : string);
    {-Write the record to a user-specified .INI file}
  var
    DestIni : IniDataBasePtr;

  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    New(DestIni, Init(IniFile, False, True));
    if DestIni = nil then begin
      AsyncStatus := ecOutOfMemory;
      Exit;
    end;

    DestIni^.DictionaryHead := DictionaryHead;
    DestIni^.DictionaryTail := DictionaryTail;

    DestIni^.PrepareIniDataBase(DefaultRecord);
    if AsyncStatus <> ecOK then
      Exit;

    DestIni^.iSaveIniRecord(Group, Rec, True);

    Dispose(DestIni, Done);
  end;

  procedure IniDataBase.ReadFromIni(var Rec; Group, IniFile : string);
    {-Read the record from a user-specified .INI file}
  var
    SourceIni : IniDataBasePtr;

  begin
    {if there are no items defined, it's an error}
    if (DictionaryHead = nil) then begin
      AsyncStatus := ecNoFieldsDefined;
      Exit;
    end;

    if not Prepared then begin
      AsyncStatus := ecDatabaseNotPrepared;
      Exit;
    end;

    New(SourceIni, Init(IniFile, False, True));
    if SourceIni = nil then begin
      AsyncStatus := ecOutOfMemory;
      Exit;
    end;

    SourceIni^.DictionaryHead := DictionaryHead;
    SourceIni^.DictionaryTail := DictionaryTail;

    SourceIni^.PrepareIniDataBase(DefaultRecord);
    if AsyncStatus <> ecOK then
      Exit;

    SourceIni^.iRetrieveIniRecord(Group, Rec, True);

    Dispose(SourceIni, Done);
  end;

end.
