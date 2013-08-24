
  {-- Support for messagebase index files ------------------------------------}
  TopicHdrRecord = record
                      NumMsgs     : Longint;
                      NumThreads  : Longint;
                      LastPoster  : String[55];
                      LastPostDate: Longint;
                      Unused      : Array[0..90] of Byte;
                   end; { record }


  TopicIdxRecord = record
                      MsgNum      : Longint;
                      TopicStarter: String[55];
                      TotalPosts  : Longint;
                      LastPoster  : String[55];
                      LastPostDate: Longint;
                      LastPostNum : Longint;       { Last message post number }
                      Attribute   : Byte;                     { bit 0: closed }
                      OrigPostDate: Longint;               { Start post topic }
                      Unused      : Array[0..86] of Byte;
                   end; { record }


