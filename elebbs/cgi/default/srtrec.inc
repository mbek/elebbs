(*
** Thread sorter records
**
** Header file for EleXer
**
** Written: 22-Jul-2002
**
**)

type
  threadSortRec = record
                    ThreadNum: Longint;                { -1 is disabled entry }
                    Attribute: Integer[1];              { Bit 0: Locked topic }
                  end; { record }

