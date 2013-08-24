UNIT ELX_GLOB;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I Compiler.inc}
{$IFDEF FPC}
  {$PACKRECORDS 1}
  {$PACKENUM 1}
{$ENDIF}
{$R-,S-}
(*
**
** Global identifiers and opcodes for Elexer
**
** Copyright (c) 2000 by Maarten Bekers
**
** Created : 26-Oct-2000
** Last update : 8-Aug-2002
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{.$DEFINE ELX_PROFILE}

uses {$IFNDEF DELPHI}Dos,{$ENDIF}
     FileObj, LongStr, StrPath, CfgRec, TimeStmp
     {$IFNDEF MSDOS}
       ,HashMap
     {$ENDIF}

     ;

const
  CompVersion  = 0.12;                    { Version number of this compiler }
  ElxInfoStr   = #8 + 'ELEXER v0.03 script module' + #10#13 + #26;
  ElxVersionID = 4;     { Version number, any other version is incompatible }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  fatal_idents     = 01;
  fatal_procedures = 02;
  fatal_reals      = 03;
  fatal_arrays     = 04;
  fatal_levels     = 05;
  fatal_code       = 06;
  fatal_strings    = 07;
  fatal_inputline  = 08;
  fatal_NulString  = 09;
  fatal_RecordSize = 10;
  fatal_DynArray   = 11;

  err_UnkIdent     = 00;                               { Unknown identifier }
  err_DupIdent     = 01;                             { Duplicate identifier }
  err_VarIdent     = 02;                     { Variable identifier expected }
  err_ProgMiss     = 03;                      { Program keyword is required }
  err_RParMiss     = 04;                                     { ")" expected }
  err_CollMiss     = 05;                                     { ":" expected }
  err_Syntax       = 06;                                     { Syntax error }
  err_NotUsed      = 07;                                         { Not used }
  err_MissOf       = 08;                                    { "OF" expected }
  err_LParMiss     = 09;                                     { "(" expected }
  err_CantPack     = 10;                      { PACKED keyword wrongly used }
  err_MissLBrk     = 11;                                     { "[" expected }
  err_MissRBrk     = 12;                                     { "]" expected }
  err_MissDtDt     = 13;                                    { ".." expected }
  err_MissSemi     = 14;                                     { ";" expected }
  err_FuncResult   = 15;                     { Invalid function result type }
  err_EqlExpect    = 16;                                     { "=" expected }
  err_BoolExpect   = 17;                      { Boolean expression expected }
  err_ForVar       = 18;                     { Invalid FOR control variable }
  err_ForMismatch  = 19;                                    { Type mismatch }
  err_NotUsed2     = 20;                                         { Not used }
  err_NumTooLarge  = 21;                          { Numeric value too large }
  err_UnexpecEof   = 22;                           { Unexpected end of file }
  err_NoCaseType   = 23;                  { Invalid type for case statement }
  err_InvChar      = 24;                                { Invalid character }
  err_ConstVar     = 25;              { Cannot use vars in const expression }
  err_IndexTyp     = 26;                     { Invalid index type for array }
  err_IndexBound   = 27;            { Incorrect parameter in index boundary }
  err_NoArray      = 28;                       { Identifier is not an array }
  err_NoType       = 29;                                    { Error in type }
  err_UndefType    = 30;                                   { Undefined type }
  err_NoRecord     = 31;                          { Identifier is no record }
  err_NoBoolean    = 32;                      { Boolean expression expected }
  err_NoArithm     = 33;                         { Arithmetic type expected }
  err_IntExpected  = 34;                            { Integer type expected }
  err_ExprType     = 35;                                    { Type mismatch }
  err_VarType      = 36;                       { Invalid var parameter type }
  err_InvVarRef    = 37;                       { Invalid variable reference }
  err_StrExpected  = 38;                       { String expression expected }
  err_NrParams     = 39;                     { Invalid number of parameters }
  err_WrongNum     = 40;                                 { Incorrect number }
  err_NoReadWrite  = 41;      { Cannot Read or Write variables of this type }
  err_MustReal     = 42;                             { Must be of type REAL }
  err_MustInt      = 43;                            { Integer type expected }
  err_VarConst     = 44;                                   { Type mismatch. }
  err_VarProc      = 45;                       { Invalid variable reference }
  err_InvAssign    = 46;                      { Type mismatch in assignment }
  err_TypeCase     = 47;             { Constant and CASE types do not match }
  err_FuncMatch    = 48;                 { Type mismatch (in internal func) }
  err_NotUsed4     = 49;                                         { Not used }
  err_InvConst     = 50;                               { Invalid const type }
  err_AsExpected   = 51;                                   { ":=" expected. }
  err_ThenExpected = 52;                                    { THEN ex3ected }
  err_UntilExpected= 53;                                   { UNTIL expected }
  err_DoExpected   = 54;                                      { DO expected }
  err_ToExpected   = 55;                            { TO or DOWNTO expected }
  err_BeginExpected= 56;                                   { BEGIN expected }
  err_EndExpected  = 57;                                     { END expected }
  err_FacExpected  = 58;                             { Factor type expected }
  err_CommaExpected= 59;                                     { "," expected }
  err_InvStrIdx    = 60;                             { Invalid string index }
  err_ArTooLarge   = 61;                              { Structure too large }
  err_ElseExpected = 62;                                    { ELSE expected }
  err_CallTabExceed= 63;                            { "CALL" table exceeded }
  err_NoNestedRec  = 64;               { Nested records/arrays not suported }
  err_DynArExpected= 65;                             { StringArray expected }
  err_UnkSubstitute= 66;               { Unknown substitution variable used }

  op_LoadAddress   = 00;                                    { Load address }
  op_LoadValue     = 01;                                      { Load value }
  op_LoadIndirect  = 02;                                   { Load indirect }
  op_UpdateDisplay = 03;                                { Update display[] }
  op_IntError_04   = 04;                             { Internal error (04) }
  op_IntError_05   = 05;                             { Internal error (05) }
  op_IntError_06   = 06;                             { Internal error (06) }
  op_Concatenate   = 07;                                   { Concatenation }
  op_RunFunction   = 08; {*}                              { Run a function }
  op_OffSet        = 09;                                          { Offset }
  op_JumpTo        = 10;                                 { Jump to address }
  op_ConditionalJmp= 11;                                { Conditional jump }
  op_Switch        = 12;                                          { Switch }
  op_IntError_13   = 13;                             { Internal error (13) }
  op_For1Up        = 14;                 { For up (begin) - run only twice }
  op_For2Up        = 15;      { For up (end) - run as many times as needed }
  op_For1Down      = 16;                                      { For 1 down }
  op_For2Down      = 17;                                      { For 2 down }
  op_MarkStack     = 18;                                      { Mark stack }
  op_Call          = 19;                                            { Call }
  op_Index1        = 20;                                          { Index1 }
  op_Index         = 21;                                           { Index }
  op_LoadBlock     = 22;                                      { Load block }
  op_CopyBlock     = 23;                                      { Copy block }
  op_Literal       = 24;                                         { Literal }
  op_LoadReal      = 25;                                       { Load real }
  op_LoadFloat     = 26;                                      { Load float }
  op_Read          = 27;                                            { Read }
  op_IntError_28   = 28;                             { Internal error (28) }
  op_Write1        = 29;                         { Write1 is (writeln(a);) }
  op_Write2        = 30;                       { Write2 is (writeln(a:1);) }
  op_CharAndString = 31;                                   { Char + String }
  op_StringRel     = 32;                                { String relations }
  op_RunUserFunc   = 33;                              { Run user func/proc }
  op_StringLink    = 34;                  { Linkup string to freeing table }
  op_DynArIndex    = 35;                                      { DynArIndex }
  op_DynArCommand  = 36;                                    { DynArCommand }
  op_DynArChild    = 37;          { Dynamic array which has dynamic childs }
  op_IntError_38   = 38;                             { Internal error (38) }
  op_IntError_39   = 39;                             { Internal error (39) }
  op_IntError_40   = 40;                             { Internal error (40) }
  op_IntError_41   = 41;                             { Internal error (41) }
  op_IntError_42   = 42;                             { Internal error (42) }
  op_IntError_43   = 43;                             { Internal error (43) }
  op_IntError_44   = 44;                             { Internal error (44) }
  op_IntError_45   = 45;                             { Internal error (45) }
  op_IntError_46   = 46;                             { Internal error (46) }
  op_IntError_47   = 47;                             { Internal error (47) }
  op_IntError_48   = 48;                             { Internal error (48) }
  op_IntError_49   = 49;                             { Internal error (49) }
  op_IntError_50   = 50;                             { Internal error (50) }
  op_IntError_51   = 51;                             { Internal error (51) }
  op_IntError_52   = 52;                             { Internal error (52) }
  op_IntError_53   = 53;                             { Internal error (53) }
  op_IntError_54   = 54;                             { Internal error (54) }
  op_IntError_55   = 55;                             { Internal error (55) }
  op_IntError_56   = 56;                             { Internal error (56) }
  op_IntError_57   = 57;                             { Internal error (57) }
  op_IntError_58   = 58;                             { Internal error (58) }
  op_IntError_59   = 59;                             { Internal error (59) }
  op_IntError_60   = 60;                             { Internal error (60) }
  op_IntError_61   = 61;                             { Internal error (61) }
  op_IntError_62   = 62;                             { Internal error (62) }
  op_IntError_63   = 63;                             { Internal error (63) }
  op_IntError_64   = 64;                             { Internal error (64) }
  op_IntError_65   = 65;                             { Internal error (65) }
  op_IntError_66   = 66;                             { Internal error (66) }
  op_IntError_67   = 67;                             { Internal error (67) }
  op_IntError_68   = 68;                             { Internal error (68) }
  op_IntError_69   = 69;                             { Internal error (69) }
  op_IntError_70   = 70;                             { Internal error (70) }
  op_IntError_71   = 71;                             { Internal error (71) }
  op_IntError_72   = 72;                             { Internal error (72) }
  op_IntError_73   = 73;                             { Internal error (73) }
  op_IntError_74   = 74;                             { Internal error (74) }
  op_IntError_75   = 75;                             { Internal error (75) }
  op_IntError_76   = 76;                             { Internal error (76) }
  op_IntError_77   = 77;                             { Internal error (77) }
  op_IntError_78   = 78;                             { Internal error (78) }
  op_IntError_79   = 79;                             { Internal error (79) }
  op_IntError_80   = 80;                             { Internal error (80) }
  op_IntError_81   = 81;                             { Internal error (81) }
  op_IntError_82   = 82;                             { Internal error (82) }
  op_IntError_83   = 83;                             { Internal error (83) }
  op_IntError_84   = 84;                             { Internal error (84) }
  op_IntError_85   = 85;                             { Internal error (85) }
  op_IntError_86   = 86;                             { Internal error (86) }
  op_IntError_87   = 87;                             { Internal error (87) }
  op_IntError_88   = 88;                             { Internal error (88) }
  op_IntError_89   = 89;                             { Internal error (89) }
  op_IntError_90   = 90;                             { Internal error (90) }
  op_IntError_91   = 91;                             { Internal error (91) }
  op_IntError_92   = 92;                             { Internal error (92) }
  op_IntError_93   = 93;                             { Internal error (93) }
  op_IntError_94   = 94;                             { Internal error (94) }
  op_IntError_95   = 95;                             { Internal error (95) }
  op_IntError_96   = 96;                             { Internal error (96) }
  op_IntError_97   = 97;                             { Internal error (97) }
  op_IntError_98   = 98;                             { Internal error (98) }
  op_IntError_99   = 99;                             { Internal error (99) }
  op_IntError_100  = 100;                           { Internal error (100) }
  op_IntError_101  = 101;                           { Internal error (101) }
  op_IntError_102  = 102;                           { Internal error (102) }
  op_IntError_103  = 103;                           { Internal error (103) }
  op_IntError_104  = 104;                           { Internal error (104) }
  op_IntError_105  = 105;                           { Internal error (105) }
  op_IntError_106  = 106;                           { Internal error (106) }
  op_IntError_107  = 107;                           { Internal error (107) }
  op_IntError_108  = 108;                           { Internal error (108) }
  op_IntError_109  = 109;                           { Internal error (109) }
  op_IntError_110  = 110;                           { Internal error (110) }
  op_IntError_111  = 111;                           { Internal error (111) }
  op_IntError_112  = 112;                           { Internal error (112) }
  op_IntError_113  = 113;                           { Internal error (113) }
  op_IntError_114  = 114;                           { Internal error (114) }
  op_IntError_115  = 115;                           { Internal error (115) }
  op_IntError_116  = 116;                           { Internal error (116) }
  op_IntError_117  = 117;                           { Internal error (117) }
  op_IntError_118  = 118;                           { Internal error (118) }
  op_IntError_119  = 119;                           { Internal error (119) }
  op_IntError_120  = 120;                           { Internal error (110) }
  op_IntError_121  = 121;                           { Internal error (121) }
  op_IntError_122  = 122;                           { Internal error (122) }
  op_IntError_123  = 123;                           { Internal error (123) }
  op_IntError_124  = 124;                           { Internal error (124) }
  op_IntError_125  = 125;                           { Internal error (125) }
  op_IntError_126  = 126;                           { Internal error (126) }
  op_IntError_127  = 127;                           { Internal error (127) }
  op_IntError_128  = 128;                           { Internal error (128) }
  op_IntError_129  = 129;                           { Internal error (129) }
  op_IntError_130  = 130;                           { Internal error (130) }
  op_Halt          = 131;                                           { Halt }
  op_ExitProc      = 132;                                 { Exit procedure }
  op_ExitFunc      = 133;                                  { Exit function }
  op_CopyArray     = 134;                                 { Ar[1] := Ar[2] }
  op_MakeBoolNot   = 135;                                    { B := NOT B; }
  op_MakeIntNegative=136;                                       { I := -I; }
  op_DecimalReal   = 137;                                { WriteLn(R:xx:2) }
  op_Store         = 138;                                          { Store }
  op_RealEql       = 139;                                          { R = R }
  op_RealNeq       = 140;                                         { R <> R }
  op_RealLss       = 141;                                          { R < R }
  op_RealLeq       = 142;                                         { R <= R }
  op_RealGtr       = 143;                                          { R > R }
  op_RealGeq       = 144;                                         { R >= R }
  op_IntEql        = 145;                                          { I = I }
  op_IntNeq        = 146;                                         { I <> I }
  op_IntLss        = 147;                                          { I < I }
  op_IntLeq        = 148;                                         { I <= I }
  op_IntGtr        = 149;                                          { I > I }
  op_IntGeq        = 150;                                         { I >= I }
  op_BoolOr        = 151;                                         { B OR B }
  op_MinInt        = 152;                                  { I1 := I1 - I2 }
  op_AddInt        = 153;                                  { I1 := I1 + I2 }
  op_MinReal       = 154;                                  { R1 := R1 - R2 }
  op_AddReal       = 155;                                  { R1 := R1 + R2 }
  op_MakeBoolAnd   = 156;                                  { B := B AND B; }
  op_MultiplyInt   = 157;                                    { I := I * I; }
  op_DivideInt     = 158;                                  { I := I DIV I; }
  op_ModInt        = 159;                                  { I := I MOD I; }
  op_MultiplyReal  = 160;                                     { R := R * R }
  op_DivideReal    = 161;                                     { R := R / R }
  op_ReadLn        = 162;                                         { ReadLn }
  op_LineFeed      = 163;                 { Linefeed (the "ln" in Writeln) }
  op_MakeRealNegative=164;                                      { R := -R; }
  op_StrIndex      = 165;                           { String index (S[xx]) }
  op_StringTemp    = 166;                               { S := 'Aa' + 'Bb' }
  op_ArToString    = 167;                        { Convert Array to string }
  op_StringAndChar = 168;                                         { S := C }
  op_StrAndStr     = 169;                                         { S := S }
  op_WriteStr      = 170;                                       { Write(s) }
  op_WriteStrTmp   = 171;                                    { Write('aa') }
  op_StrValueParam = 172;      { String as a value, parameter in func/proc }
  op_StrValParamTmp= 173;  { String as a value (temp), param. in func/proc }
  op_CharArAndStr  = 174;                            { CharArray := String }
  op_CharArAndStrTmp=175;                          { CharArray := 'String' }
  op_WriteStr2     = 176;                     { Write2 is (writeln(s:16);) }
  op_WriteStr2Tmp  = 177;             { Write2 is (writeln('Maarten':16);) }
  op_IntError_178  = 178;                           { Internal error (178) }
  op_IntError_179  = 179;                           { Internal error (179) }
  op_IntError_180  = 180;                           { Internal error (180) }
  op_IntError_181  = 181;                           { Internal error (181) }
  op_IntError_182  = 182;                           { Internal error (182) }
  op_IntError_183  = 183;                           { Internal error (183) }
  op_IntError_184  = 184;                           { Internal error (184) }
  op_IntError_185  = 185;                           { Internal error (185) }
  op_IntError_186  = 186;                           { Internal error (186) }
  op_IntError_187  = 187;                           { Internal error (187) }
  op_IntError_188  = 188;                           { Internal error (188) }
  op_IntError_189  = 189;                           { Internal error (189) }
  op_IntError_190  = 190;                           { Internal error (190) }
  op_IntError_191  = 191;                           { Internal error (191) }
  op_IntError_192  = 192;                           { Internal error (192) }
  op_IntError_193  = 193;                           { Internal error (193) }
  op_IntError_194  = 194;                           { Internal error (194) }
  op_IntError_195  = 195;                           { Internal error (195) }
  op_IntError_196  = 196;                           { Internal error (196) }
  op_IntError_197  = 197;                           { Internal error (197) }
  op_IntError_198  = 198;                           { Internal error (198) }
  op_IntError_199  = 199;                           { Internal error (199) }
  op_IntError_200  = 200;                           { Internal error (200) }
  op_IntError_201  = 201;                           { Internal error (201) }
  op_IntError_202  = 202;                           { Internal error (202) }
  op_IntError_203  = 203;                           { Internal error (203) }
  op_IntError_204  = 204;                           { Internal error (204) }
  op_IntError_205  = 205;                           { Internal error (205) }
  op_IntError_206  = 206;                           { Internal error (206) }
  op_IntError_207  = 207;                           { Internal error (207) }
  op_IntError_208  = 208;                           { Internal error (208) }
  op_IntError_209  = 209;                           { Internal error (209) }
  op_IntError_210  = 210;                           { Internal error (210) }
  op_IntError_211  = 211;                           { Internal error (211) }
  op_IntError_212  = 212;                           { Internal error (212) }
  op_IntError_213  = 213;                           { Internal error (213) }
  op_IntError_214  = 214;                           { Internal error (214) }
  op_IntError_215  = 215;                           { Internal error (215) }
  op_IntError_216  = 216;                           { Internal error (216) }
  op_IntError_217  = 217;                           { Internal error (217) }
  op_IntError_218  = 218;                           { Internal error (218) }
  op_IntError_219  = 219;                           { Internal error (219) }
  op_IntError_220  = 220;                           { Internal error (220) }
  op_IntError_221  = 221;                           { Internal error (221) }
  op_IntError_222  = 222;                           { Internal error (222) }
  op_IntError_223  = 223;                           { Internal error (223) }
  op_IntError_224  = 224;                           { Internal error (224) }
  op_IntError_225  = 225;                           { Internal error (225) }
  op_IntError_226  = 226;                           { Internal error (226) }
  op_IntError_227  = 227;                           { Internal error (227) }
  op_IntError_228  = 228;                           { Internal error (228) }
  op_IntError_229  = 229;                           { Internal error (229) }
  op_IntError_230  = 230;                           { Internal error (230) }
  op_IntError_231  = 231;                           { Internal error (231) }
  op_IntError_232  = 232;                           { Internal error (232) }
  op_IntError_233  = 233;                           { Internal error (233) }
  op_IntError_234  = 234;                           { Internal error (234) }
  op_IntError_235  = 235;                           { Internal error (235) }
  op_IntError_236  = 236;                           { Internal error (236) }
  op_IntError_237  = 237;                           { Internal error (237) }
  op_IntError_238  = 238;                           { Internal error (238) }
  op_IntError_239  = 239;                           { Internal error (239) }
  op_IntError_240  = 240;                           { Internal error (240) }
  op_IntError_241  = 241;                           { Internal error (241) }
  op_IntError_242  = 242;                           { Internal error (242) }
  op_IntError_243  = 243;                           { Internal error (243) }
  op_IntError_244  = 244;                           { Internal error (244) }
  op_IntError_245  = 245;                           { Internal error (245) }
  op_IntError_246  = 246;                           { Internal error (246) }
  op_IntError_247  = 247;                           { Internal error (247) }
  op_IntError_248  = 248;                           { Internal error (248) }
  op_IntError_249  = 249;                           { Internal error (249) }
  op_IntError_250  = 250;                           { Internal error (250) }
  op_IntError_251  = 251;                           { Internal error (251) }
  op_IntError_252  = 252;                           { Internal error (252) }
  op_IntError_253  = 253;                           { Internal error (253) }
  op_IntError_254  = 254;                           { Internal error (254) }
  op_IntError_255  = 255;                           { Internal error (255) }

const
  NrKeyws   = 36;                                      { Number of keywords }
  IdLen     = 30;              { Number of significant chars in identifiers }
  MaxLineLen= 242;                                      { Input line length }

  MaxRecByteSize = 1024 * 32;                   { Max. bytesize of a record }

  {$IFDEF MSDOS}
    MaxSyms     = 900;                               { Size of symbol table }
    MaxBlock    = 30;                                 { Size of block-table }
    MaxArray    = 30;                                 { Size of array-table }
    MaxReals    = 50;                         { Size of real constant table }
    MaxCases    = 30;                        { Max. number of case elements }
    MaxCode     = 2000;                                { Size of code table }
    MaxLevel    = 50;           { Maximum level of procedure/record nesting }
    MaxError    = 66;                          { Maximum error number (???) }
    MaxDynArray = 30;             { Maximum number of dynamic string arrays }

    StackSize = 1500;                                 { Size of stack-table }
  {$ELSE}
    MaxSyms     = 3000;
    MaxBlock    = 500;                                { Size of block-table }
    MaxArray    = 500;                                { Size of array-table }
    MaxReals    = 150;                        { Size of real constant table }
    MaxCases    = 50;                        { Max. number of case elements }
    MaxCode     = 95000;                               { Size of code table }
    MaxLevel    = 150;          { Maximum level of procedure/record nesting }
    MaxError    = 66;                          { Maximum error number (???) }
    MaxDynArray = 50;             { Maximum number of dynamic string arrays }

    StackSize = 25000;                                { Size of stack-table }
  {$ENDIF}

  MaxOrder  = 900;                     { Highest order code (sort of func?) }
  MaxArBound= 32767;                                  { Maximum array bound }
  MaxInteger= 2147483647;                            { Maximum integer size }

{$IFDEF MSDOS}
  MaxStrings = 1000;                                      { Max. strings }
{$ELSE}
  MaxStrings = 4000;
{$ENDIF}

const
  ReadMode      = 0;                         { Constants for System.FileMode }
  WriteMode     = 1;
  ReadWriteMode = 2;

  DenyAll       = $10;
  DenyWrite     = $20;
  DenyRead      = $30;
  DenyNone      = $40;

type
  tSymbol   = (sym_intcon,    sym_realcon,   sym_charcon,  sym_stringcon,
               sym_not,       sym_plus,      sym_minus,    sym_times,
               sym_idiv,      sym_rdiv,      sym_imod,     sym_and,
               sym_or,        sym_in,        sym_eql,      sym_neq,
               sym_gtr,       sym_geq,       sym_lss,      sym_leq,
               sym_lparent,   sym_rparent,   sym_lbrack,   sym_rbrack,
               sym_comma,     sym_semicolon, sym_period,   sym_twodots,
               sym_colon,     sym_becomes,   sym_const,    sym_type,
               sym_var,       sym_func,      sym_nil,      sym_proc,
               sym_file,      sym_array,     sym_record,   sym_packed,
               sym_set,       sym_program,   sym_label,    sym_ident,
               sym_with,      sym_begin,     sym_if,       sym_case,
               sym_repeat,    sym_while,     sym_for,      sym_goto,
               sym_end,       sym_else,      sym_until,    sym_of,
               sym_do,        sym_to,        sym_downto,   sym_then,
               sym_dynarray);

  tObject   = (obj_Constant,  obj_Variable,  obj_type,
               obj_Procedure, obj_function);

  tType     = (typ_notyp,     typ_ints,      typ_reals,    typ_bools,
               typ_chars,     typ_strngs,    typ_arrays,   typ_records,
               typ_dynarray);

  IndexType = -MaxArBound..+MaxArBound;
  AlfaType  = Array[1..IdLen] of Char;          { Array to hold identifiers }

  SymbolSet = Set of tSymbol;                           { Set with symbols }
  TypeSet   = Set of tType;
  OpCodeType= record
                Func  : 0..MaxOrder;                     { Function call # }
                X     : SmallInt; { integer; 0..MaxLevel; }
                Y     : Longint;
              end; { OpCodeType }

  StrPtrRecord = record
                    Len    : Longint;
                    {$IFDEF MSDOS}
                      Content: String[250];
                    {$ELSE}
                       Content : ^Char;
                       Alloced : LongBool;
                       AllocSz : Longint;
                    {$ENDIF}
                 end; { record }

type
  FileTableRecord = record
                      Used     : LongBool;
                      LastError: Longint;
                      FilePtr  : pFileObj;
                    end; { FileTableRecord }
const
  MaxFileTable = 30;


type
  {-- The reason why typ_real is pre-filled is because it else could -----}
  {-- leave lingering data in the linked list for strings. This is the ---}
  {-- easiest way to work around it, but a proper fix would be cooler ----}
  StackType = record                          { blockmark:                }
                DynArraySet: Integer; { used for dynamic arrays, when }
                                      { set to true, it shouldnt be linked }
                                      { like other strings }

                case CN : tType of            { stack[b+0] = fct result   }
                  typ_ints:  (  i: Longint);  { stack[b+1] = return adr   }
                  typ_reals: (dum,dum2: longint;
                              r: real);       { stack[b+2] = static link  }
                  typ_bools: (  b: boolean);  { stack[b+3] = dynamic link }
                  typ_chars: (  c: char);     { stack[b+4] = table index  }
                  typ_strngs:(s,p: Longint);  { stack[b+5] = string ptr   }
                end; { case }                 { stack[b+6] = dynarray ptr }

  StackArray = Array[0..StackSize] of StackType;

type
  ArrayInfoRec = record
                   IdxTyp        : tType;                   { Index type }
                   ElementType   : tType;

                   Low           : IndexType;       { Low bound of array }
                   High          : IndexType;      { High bound of array }
                   ElementRef    : IndexType;                    { ????? }
                   ElementSize   : IndexType; { Number of stack entries it uses }
                   ElementMaxSize: IndexType;{ "Real" size of this element }
                   TotalSize     : IndexType;      { Total size of array }
                 end; { record }

type
  ArrayHolderObj = object
                      ArrayInf: ArrayInfoRec;

                      constructor Init;
                      destructor Done;
                   end; { object }
  pArrayHolderObj = ^ArrayHolderObj;

{$IFNDEF MSDOS}
type
  pNulIntArray = Array[0..0] of StackType;
  DynArrayRecord= record
                    ArrayPtrRec  : ^PNulIntArray;
                    ArrayNames   : THashMap;
                    alloc_High   : Integer;     { Highest element allocated }
                    LastElementLd: Integer;  { Last element that was loaded }

                    HighestItem  : Integer; { Highest element actually used }
                    LowestItem   : Integer;  { Lowest element actually used }

                    ElementType   : tType;
                    ElementRef    : IndexType;                    { ????? }
                    ElementSize   : IndexType; { Number of stack entries it uses }
                    ElementMaxSize: IndexType;{ "Real" size of this element }
                    TotalSize     : IndexType;      { Total size of array }
                    ChildIsDynamic: Boolean;    { If child is also a dyn ar }
                  end; { record }
{$ENDIF}

type
  IdentRecord = packed record
                  Name       : AlfaType;            { Name of identifier }
                  Link       : IndexType;    { Linked list type of thing }
                  Obj        : tObject;            { Sort of declaration }
                  Typ        : tType;              { Sort of return code }
                  Ref        : IndexType;                        { ????? }
                  Normal     : Boolean;        { Pass as var or "normal" }
                  Lev        : 0..MaxLevel;{ Level of visibility (in display) }
                  Adr        : Longint; { Address (unique) for this display }
                  MaxSize    : Longint;    { Maximum size of declaration }
                end; { record }

  IdentArray = Array[0..maxSyms] of IdentRecord;
  ProgCodeArray = Array[0..MaxCode] of OpCodeType;
  ArrayType = Array[1..MaxArray] of pArrayHolderObj;        { Array table }
  {$IFNDEF MSDOS}
    DynArrayType = Array[1..MaxDynArray] of DynArrayRecord;
  {$ENDIF}

  BlockType = Array[1..MaxBlock] of                        { Block table }
                    record
                      Last       : IndexType;   { Last ident. in this block }
                      LastPar    : IndexType;                       { ????? }
                      pSize      : IndexType;          { Size of this block }
                      vSize      : IndexType;                       { ????? }
                      ByteSize   : IndexType;  { Size (in bytes!) that this }
                    end; { record }

  DisplayType = Array[0..MaxLevel] of Longint;
  BaseZeroType = Array[0..MaxLevel] of Longint;

  StringArrayType = Array[0..MaxStrings] of ^StrPtrRecord;

{ Writing the module file to disk is rather complex. In order to allow }
{ for expansion, all records are variable of size. We can also later add }
{ other blocks which also need to be read.  }

{ The module file, always consists of the header, and after that a }
{ number of seperate modules. }

const
  elx_Mod_Display      = $00;
  elx_Mod_IdentTable   = $01;
  elx_Mod_ArrayTable   = $02;
  elx_Mod_BlockTable   = $03;
  elx_Mod_RealsTable   = $04;
  elx_Mod_StrTable     = $05;
  elx_Mod_CallList     = $06;
  elx_Mod_DynArrayTable= $07;
  elx_Mod_ProgCode     = $FF;



type
  elx_ModHdr = record                                    { Header module }
                  ElxInfo  : String[50];    { Elexer v0.01 script module }
                  Version  : Longint;                            { $0001 }
                  ModName  : String[20];                   { Module name }
                  DateTime : String[20];            { 12-Jun-1980, 12:12 }
                  StartBlck: Longint;      { Start block for interpreter }
               end; { record }

  elx_Module  = record
                  ElxTyp  : Longint;                 { (see table above) }
                  NrRecs  : Longint;        { Number of records well use }
                end; { record }

  elx_StrModule = record
                    StrItem: Longint;               { String item number }
                    Size   : Longint;             { Length of the string }
                  end; { record }

{-- User hooks for the programmer ------------------------------------------}
type
  elx_AddUserFuncsTyp = procedure(var elx_GlobalBuf; OnlyParamList: Boolean);
  elx_UserFuncsTyp    = procedure(const FuncNr: Longint;
                                  var   elx_GlobalsBuf;
                                  var   TmpStack: Array of StackType;
                                  var   FuncReturn: StackType;
                                  var   Objptr: pointer;
                                  const CallTypes: String);
  elx_SystemFuncsTyp  = procedure(const FuncNr: Longint;
                                  var   elx_GlobalsBuf;
                                  var   TmpStack: Array of StackType;
                                  var   CallPtr: Pointer;
                                  var   FuncReturn: StackType);
  {$IFDEF MSDOS}
    elx_UserWriteTyp    = procedure(const TmpStr: String; DoLn: Boolean);
  {$ELSE}
    elx_UserWriteTyp    = procedure(const TmpStr: AnsiString; DoLn: Boolean);
  {$ENDIF}
  elx_UserReadTyp     = procedure(var TmpStr; typ: tType; DoLn: Boolean);


const
  MaxParamList = 700;       { Maximum number of programmer added parameters }
  maxCallList  = 700;       { Maximal times a prog-supplied function can be called }

type
  UserCallListType  = Array[0..maxParamList] of ^String;
  UserParamListType = Array[0..maxCallList] of ^String;

type
  BlockRec = record
               LastCH       : Char;    { Last character read from source file }
               CharCount    : Longint;   { Character counter (column counter) }
               CurLineLen   : Longint;               { Length of current line }
               ErrorPos     : Longint;           { Column where error occured }
               CurLine      : String;                          { Current line }

               inSym_Real   : Real;               { Real number from InSymbol }
               inSym_Integer: Longint;                { Integer from InSymbol }

               StringPtr    : Longint;             { Work pointer to a string }

                                { Encountered an error, display which symbols }
               SkipFlag     : Boolean;                       { are unexpected }

               CurIdent     : AlfaType;            { Identifier from InSymbol }
               CurSymbol    : tSymbol;         { Last symbol read by InSymbol }

               ConstBegSys  : SymbolSet;    { Symbols allowed in CONST blocks }
               TypeBegSys   : SymbolSet;     { Symbols allowed in TYPE blocks }
               BlockBegSys  : SymbolSet; { Symbls allowed when defining blocks }
               FacBegSys    : SymbolSet;                                         { ????? }
               StatBegSys   : SymbolSet;                                         { ????? }

               KwName       : Array[1..NrKeyws] of AlfaType;{ Array of all keywords }
               KwSymbol     : Array[1..NrKeyws] of tSymbol;{ Matching symbol type }
               SpecialChars : Array['!'..'~'] of tSymbol; { Special characters }
             end; { record }


type
  elx_GlobalsType = record
    BlockVars    : ^BlockRec;     { Variables we need for the compiler itself }
    LogErrors    : Boolean;            { Log errors (Stack dump, etc) to file }
    AbortCompile : Boolean;                               { Abort compilation }
    ErrorName    : String;                            { Filename of errorfile }
    Stack        : StackArray;                             { "The" stack :-) }
    elx_ParamStr : String;                     { The parameters to the script }
    IdentCount   : Longint;                               { Index to id-table }
    HighIdentCount:Longint;                          { Highest identcount saw }
    StartBlock   : Longint;         { Block count to start at for interpreter }
    StackDump    : Boolean;          { Dump the stack after calling procedure }
    HadErrors    : Boolean;                            { Compiled with errors }
    PrTables     : Boolean;     { Print tables (block, array etc) at the end? }
    NulString    : Longint;                        { Segment:0 of null string }
    CurCallList  : Longint;                     { Currently called user funcs }
    ModuleName   : String;                              { Value of PROGRAM id }
    FileTable    : Array[1..maxFileTable] of FileTableRecord;   { used by int }
    BaseZeroArray: ^BaseZeroType;                  { Array of basedzero saves }
    BaseZeroIdx  : Longint;
    Display      : ^DisplayType;
    IdentTable   : ^IdentArray;                           { Identifier table }
    StringTable  : ^StringArrayType;
    StrLastFreed : Array[0..9] of Longint; {!!}
    StrLastIndex : Longint;      { Index to last string found in full search }

    ArrayTable   : ^ArrayType;
    {$IFNDEF MSDOS}
      DynArrayTable: ^DynArrayType;
    {$ENDIF}
    BlockTable   : ^BlockType;
    UserParamList: ^UserParamListType;                 { User supplied params }
    UserCallList : ^UserCallListType;                   { Parameters supplied }
    StandardTypes: TypeSet;                                  { Standard types }
    RealsTable   : Array[1..MaxReals] of Real;                   { Real table }
    ProgCode     : ProgCodeArray;                       { Actual program code }
    ErrorSet     : Array[0..MaxError] of Byte; { Errors encountered while compiling }

    CurCode      : Longint;                           { Current code# counter }
    ArrayCount   : Longint;                            { Index to Array-table }
    DynArrayCount: Longint;           { Amount of string arrays actually used }
    BlockCount   : Longint;                            { Index to block-table }
    RealCount1   : Longint;{ Realcount1 is used to avoid dups in the real tbl }
    RealCount2   : Longint;          { Realcount2 is the number of used reals }

    InitialStrCnt: Longint;                  { initial highest string defined }

    {-- Interpreter variables ------------------------------------------------}
    int_Continue : Boolean;                                    { Keep running }
    int_ProgState: (run,                                       { Running fine }
                    fin,                                           { All thru }
                    stkchk,                              { Out of stack space }
                    caschk,                          { Missing case statement }
                    divchk,                                  { Divide by zero }
                    inxchk,                             { Invalid index value }
                    redchk,                        { Reading past End Of file }
                    strchk,                      { Unable to get a new string }
                    fnchk,                 { Invalid value passed to function }
                    syschk,                       { Internal (compiler) error }
                    basezchk);                         { baseZero array limit }
    int_H1,                                              { Temporarily values }
    int_H2,                                              { Temporarily values }
    int_H3,                                              { Temporarily values }
    int_H4,                                              { Temporarily values }
    int_H5,                                              { Temporarily values }
    int_H6       : Longint;                              { Temporarily values }
    int_PrevPC   : longint;
    int_OpCode   : Longint;
    int_Param1   : Longint;
    int_ProgPtr  : Longint;                                { Program counter }
    int_Param2   : Longint;
    int_TmpStack : Array[0..5] of StackType;
    int_BaseZero : Longint;    { Base index for the current calling procedure }
    int_BaseIndex: Longint;                           { Current stack pointer }
    int_ErrorStr : String;                    { Error string from interpreter }
    {$IFNDEF MSDOS}
      int_TmpStr   : AnsiString;
    {$ELSE}
      int_TmpStr   : String;
    {$ENDIF}
    int_CallObjPtr:Pointer;


    {-- The source and error files, initialized by main program --------------}
    ErrorFile    : pFileObj;
    CurSrcFile   : Integer;
    SourceFile   : Array[0..10] of record
                                     SrcFile   : pFileObj;
                                     SrcName   : String;
                                     CurLineNum: Longint;
                                   end; { record }
    ModHdr       : elx_ModHdr;
    elx_OnlyParamList: Boolean;      { Only add identifiers for the paramlist }

    elx_AddUserFuncsHook : elx_AddUserFuncsTyp;
    elx_UserFuncsHook    : elx_UserFuncsTyp;
    elx_SystemFuncsHook  : elx_SystemFuncsTyp;
    elx_UserWriteHook    : elx_UserWriteTyp;
    elx_UserReadHook     : elx_UserReadTyp;
  end; { elx_GlobalsType }


type
  CallProcType = procedure(var elx_Globals: elx_GlobalsType);


{$IFDEF MSDOS}
{procedure StrSetString(var elx_Globals: elx_GlobalsType; StrIndex: Longint; InsStr: String; Len: Longint);}
function  StrGetString(var elx_Globals: elx_GlobalsType; StrIndex, Pos, Len: Longint): String;
{$ELSE}
{procedure StrSetString(var elx_Globals: elx_GlobalsType; StrIndex: Longint; InsStr: AnsiString; Len: Longint);}
function  StrGetString(var elx_Globals: elx_GlobalsType; StrIndex, Pos, Len: Longint): AnsiString;
{$ENDIF}
procedure StrFreeString(var elx_Globals: elx_GlobalsType; var StrPtr: Longint);
procedure StrReleaseString(var elx_Globals: elx_GlobalsType; var StrPtr: Longint);
{procedure StrSetStrLen(var elx_Globals: elx_GlobalsType; var StrPtr: Longint; Len: Longint); }
{function  StrGetStrLen(var elx_Globals: elx_GlobalsType; var StrPtr: Longint): Longint;}
{function  StrGetStrSize(var elx_Globals: elx_GlobalsType; var StrPtr: Longint): Longint;}
function  StrAllocNew(var elx_Globals: elx_GlobalsType; var StrPtr: Longint; T: Longint): Boolean;
function StrGetFreeCount(var elx_Globals: elx_GlobalsType): Longint;
function  StrGetNearSize(Len: Longint): Longint;
procedure StrGrowString(var elx_Globals: elx_GlobalsType; var StrPtr: Longint; T: Longint);
{$IFNDEF MSDOS}
procedure DynArrayFreeIt(var elx_Globals: elx_GlobalsType; const DynArray: Longint; FullFree: Boolean);
procedure DynArrayPutElement(var elx_Globals: elx_GlobalsType;
                             var ArrayRef, ArrayIdx: Longint;
                             var StackPos: Longint);
{$ENDIF}

procedure GlobalFreeMemory(var elx_Globals: elx_GlobalsType; EndExit, FreeModule: Boolean);
procedure elx_AddUserFuncs(var elx_GlobalBuf; OnlyParamList: Boolean);

function HasErrors(var elx_Globals: elx_GlobalsType): Boolean;
function CreateDateTime: String;
function GlobalInitialize(var elx_Globals: elx_GlobalsType): Boolean; { init vars used both by compiler and int }
function IntStr(N, Len: Longint): String;             { Convert int to str }
function GetFieldSize(var elx_Globals: elx_GlobalsType; var Ident: IdentRecord): Longint;

function ElxModName(FName: String): String;
function WriteModuleToDisk(var elx_Globals: elx_GlobalsType; FName: String): Boolean;      { Write to disk }
function ReadModuleFromDisk(var elx_Globals: elx_GlobalsType; FName: String): Boolean;    { Read from disk }
function OpCodeToStr(const OpCode: Longint): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses Elx_Blck
        {$IFNDEF MSDOS}
          ,SysUtils
          ,Classes
        {$ELSE}
          ,Strings
        {$ENDIF} ;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
procedure DynArrayInit(var elx_Globals: elx_GlobalsType; const DynArray: Longint);
var NewSize: Longint;

ick: real;
ick2: real;
begin
  with elx_Globals.DynArrayTable^[DynArray] do
    begin
      {-- Initialize this array ----------------------------------------}
      NewSize := 256;
      // does not work when using with records  (!? need fix!)
      
      if ElementSize < 1 then
         ElementSize := 1;
      
      //writeln('ElementSize: ', ElementSize);

{$ifdef elx_profile}
  ick:=getmicroseconds;      
{$endif}
      {-- allocate all new pointer entries -----------------------------}
      GetMem(ArrayPtrRec, (NewSize * (SizeOf(ArrayPtrRec^[0]) * ElementSize) ));
{$ifdef elx_profile}
  writeln('getmem:   ', DynArray, ' -->> ', real(getmicroseconds - ick):3:8, ' -> ', NewSize, ' - > ', (NewSize * (SizeOf(ArrayPtrRec^[0]) * ElementSize) ));      
{$endif}

      {-- and initialize it --------------------------------------------}
{$ifdef elx_profile}
  ick:=getmicroseconds;      
{$endif}
      FillChar(ArrayPtrRec^[0], (NewSize * (SizeOf(ArrayPtrRec^[0]) * ElementSize) ), 0);
{$ifdef elx_profile}
  writeln('FillChar: ', DynArray, ' -->> ', real(getmicroseconds - ick):3:8);      
{$endif}


(*
ArrayPtrRec^[0].S := 0;

if dynarray >= 3 then
writeln('initting array: ', dynarray, ' but ...: ', elx_Globals.DynarrayTable^[3].ArrayPtrRec^[0].s, ' -> ', elx_Globals.DynarrayTable^[3].ArrayPtrRec^[1].s,
' -> ', elementsize);
*)

      {-- initialize the hashmap ---------------------------------------}
{$ifdef elx_profile}
  ick:=getmicroseconds;      
{$endif}
      ArrayNames := THashMap.Create(NewSize);
{$ifdef elx_profile}
  writeln('Hashmap: ', DynArray, '  -->> ', real(getmicroseconds - ick):3:8);      
{$endif}

      {-- and update the new array -------------------------------------}
      alloc_High := NewSize;

(*
with elx_globals.dynarraytable^[dynarray] do
begin
writeln('blaat[', DynArray, '].alloc = ', alloc_high);
writeln('blaat[', DynArray, '].lastelemendld = ', lastelementld);
writeln('blaat[', DynArray, '].highestitem = ', highestitem);
writeln('blaat[', DynArray, '].lowestitem = ', lowestitem);
writeln('blaat[', DynArray, '].totalsize = ', totalsize);
end;
*)

    end; { if }
end; { proc. DynArrayInit }


procedure DynArrayFreeIt(var elx_Globals: elx_GlobalsType; const DynArray: Longint; FullFree: Boolean);
var Counter: Longint;
    TempInt: StackType;
begin
  {-- and now free them individually ---------------------------------------}
  with elx_Globals.DynArrayTable^[DynArray] do
  if alloc_High > 0 then
    for Counter := 0 to (alloc_High - 1) do
      begin
        {-- get our string item --------------------------------------------}
        with elx_Globals do
          TempInt := DynArrayTable^[DynArray].ArrayPtrRec^[Counter];

        if TempInt.S <> 0 then
          StrFreeString(elx_Globals, TempInt.S);

        {-- and free this item ---------------------------------------------}
        with elx_Globals do
         if DynArrayTable^[DynArray].ArrayPtrRec <> nil then
          DynArrayTable^[DynArray].ArrayPtrRec^[Counter].S := 0;
      end; { while }

  {-- reset some variables -------------------------------------------------}
  with elx_Globals.DynArrayTable^[DynArray] do
    begin
      {-- and reset the last string set ------------------------------------}
      LastElementLd := -1;
      HighestItem := 0;
      LowestItem := MaxLongInt;
      TotalSize := 0;
    end; { with }

(*
with elx_globals.dynarraytable^[dynarray] do
begin
writeln('blaat[', DynArray, '].alloc = ', alloc_high);
writeln('blaat[', DynArray, '].lastelemendld = ', lastelementld);
writeln('blaat[', DynArray, '].highestitem = ', highestitem);
writeln('blaat[', DynArray, '].lowestitem = ', lowestitem);
writeln('blaat[', DynArray, '].totalsize = ', totalsize);
end;
*)

  {-- if this is a full free (eg: release ALL memory) we destroy it totally-}
  if FullFree then
   with elx_Globals.DynArrayTable^[DynArray] do
    if alloc_High > 0 then
      begin
       if ElementSize > 0 then      	
        if ArrayPtrRec <> nil then
          begin
            FreeMem(ArrayPtrRec, (alloc_high * (SizeOf(ArrayPtrRec^[0]) * ElementSize) ));
            ArrayPtrRec := nil;
          end; { if }
       
       if ArrayNames <> nil then
         begin
           ArrayNames.Free;
           ArrayNames := nil;
         end; { if }

       alloc_high := -1;
     end; { with }
end; { proc. DynArrayFreeIt }


procedure DynArrayPutElement(var elx_Globals: elx_GlobalsType;
                             var ArrayRef, ArrayIdx: Longint;
                             var StackPos: Longint);
var Counter,
    NewSize    : Longint;
begin
  with elx_Globals.DynArrayTable^[ArrayRef], elx_Globals do
    begin
      {-- make sure we dont need to grow -----------------------------------}
      if alloc_High <= ArrayIdx then
        begin
          {-- pre-grow the array -------------------------------------------}
          NewSize := ((alloc_High DIV 1024) + 1) * 1024;

          {-- we try to be smart. if this array is so large, we assume it --}
          {-- will grow even larger, and we start growing the array by 50% -}
          {-- instead of simple 1k blocks ----------------------------------}
          if NewSize > (1024 * 32) then
            NewSize := NewSize + (NewSize div 2);

          {-- allocate all new pointer entries -----------------------------}
          ReAllocMem(ArrayPtrRec, (NewSize * (SizeOf(ArrayPtrRec^[0]) * ElementSize) ));

          {-- and update the new array -------------------------------------}
          alloc_High := NewSize;
        end; { if }

      {-- and actually update the data -------------------------------------}
      Move(Stack[StackPos],                                  { actual stack }
           ArrayPtrRec^[ArrayIdx *
                         ElementSize],                      { array pointer }
           (ElementSize * SizeOf(Stack[1])));
    end; { with }
end; { proc. DynArrayPutElement }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StrSetStrLen(var elx_Globals: elx_GlobalsType; var StrPtr: Longint; Len: Longint);
begin
  with elx_Globals do
    StringTable^[StrPtr]^.Len := Len;
end; { proc. StrSetStrLen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrGetStrLen(var elx_Globals: elx_GlobalsType; var StrPtr: Longint): Longint;
begin
  StrGetStrLen := elx_Globals.StringTable^[StrPtr]^.Len;
end; { func. StrGetStrLen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  StrGetNearSize(Len: Longint): Longint;
begin
  StrGetNearSize := ((Len DIV 1024 + 1) * 2048);
end; { func. StrGetNearSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrGetStrSize(var elx_Globals: elx_GlobalsType; var StrPtr: Longint): Longint;
begin
  with elx_Globals do
    begin
      if StrPtr = 0 then    { If its a nul string, its 0, else return the len }
        StrGetStrSize := 0
          else begin
                 StrGetStrSize := 1; { some routines depend on it }
               end; { else }
    end; { with }
end; { func. StrGetStrSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure StrSetString(var elx_Globals: elx_GlobalsType; StrIndex: Longint; InsStr: String; Len: Longint);
{$ELSE}
procedure StrSetString(var elx_Globals: elx_GlobalsType; StrIndex: Longint; InsStr: AnsiString; Len: Longint);
{$ENDIF}
begin
  with elx_Globals do
    begin
      {$IFDEF MSDOS}
        StringTable^[StrIndex]^.Content[0] := Chr(Len);
        StringTable^[StrIndex]^.Content := InsStr;
      {$ELSE}
        StrPCopy(PChar(StringTable^[StrIndex]^.Content), InsStr);
      {$ENDIF}
    end; { with }
end; { proc. StrSetString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
function StrGetString(var elx_Globals: elx_GlobalsType; StrIndex, Pos, Len: Longint): String;
{$ELSE}
function StrGetString(var elx_Globals: elx_GlobalsType; StrIndex, Pos, Len: Longint): AnsiString;
{$ENDIF}
begin
  with elx_Globals do
    begin
      if (StrIndex = 0) then
       StrGetString := ''
          else begin
                  if Len < 1 then
                    Len := StringTable^[StrIndex]^.Len;

                  if StringTable^[StrIndex]^.Len > 0 then
                   {$IFDEF MSDOS}
                     StrGetString := Copy(StringTable^[StrIndex]^.Content, Pos, Len)
                   {$ELSE}
                     begin
                       Result := Copy(StrPas(PChar(StringTable^[StrIndex]^.Content)), Pos, Len);
                     end
                   {$ENDIF}
                       else StrGetString := '';
               end; { else }
    end; { with }
end; { func. StrGetString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StrFreeString(var elx_Globals: elx_GlobalsType; var StrPtr: Longint);
var Counter: Longint;
begin
  with elx_Globals do
    begin
      for Counter := 0 to High(StrLastFreed) do
        if StrLastFreed[Counter] <= 0 then
         if elx_Globals.StringTable^[StrPtr] <> nil then
          begin
            StrLastFreed[Counter] := StrPtr;
            BREAK;
          end; { if }
    end; { with }


  {$IFDEF MSDOS}
    if elx_Globals.StringTable^[StrPtr] <> nil then
      begin
        FreeMem(elx_Globals.StringTable^[StrPtr], SizeOf(StrPtrRecord));
        elx_Globals.StringTable^[StrPtr] := nil;
      end; { if }
  {$ELSE}
    if elx_Globals.StringTable^[StrPtr] <> nil then
      begin
        elx_Globals.StringTable^[StrPtr]^.Alloced := false;
        PChar(elx_Globals.StringTable^[StrPtr]^.Content)[0] := #0;
        elx_Globals.StringTable^[StrPtr]^.Len := 0;

{        elx_Globals.StringTable^[StrPtr]^.AllocSz := 1024; }

{        SetLength(elx_Globals.StringTable^[StrPtr].Content,
                  elx_Globals.StringTable^[StrPtr].AllocSz); }
      end; { if }
  {$ENDIF}
end; { proc. StrFreeString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StrReleaseString(var elx_Globals: elx_GlobalsType; var StrPtr: Longint);
var Counter: Longint;
begin
  if elx_Globals.StringTable^[StrPtr] <> nil then
    begin
      if elx_Globals.StringTable^[StrPtr].AllocSz <> - 1 then
        FreeMem(elx_Globals.StringTable^[StrPtr]^.Content,
                elx_Globals.StringTable^[StrPtr].AllocSz);

      elx_Globals.StringTable^[StrPtr]^.Alloced := false;
      elx_Globals.StringTable^[StrPtr].AllocSz := -1;
      elx_Globals.StringTable^[StrPtr]^.Len := 0;
    end; { if }
end; { proc. StrReleaseString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StrGrowString(var elx_Globals: elx_GlobalsType; var StrPtr: Longint; T: Longint);
begin
  {$IFNDEF MSDOS}
    elx_Globals.StringTable^[StrPtr]^.AllocSz := StrGetNearSize(T);

    ReAllocMem(elx_Globals.StringTable^[StrPtr]^.Content,
               elx_Globals.StringTable^[StrPtr]^.AllocSz);
{writeln('reallocating memory for: ', strptr);}
  {$ENDIF}
end; { proc. StrGrowString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrGetFreeCount(var elx_Globals: elx_GlobalsType): Longint;
var Counter  : Longint;
    TotalCnt : Longint;
begin
  {-- initialize variables ----------------------------------------------}
  TotalCnt := 0;
  Counter := 0;

  while (Counter < MaxStrings) do
   with elx_Globals do
    begin
       Inc(Counter);

       if StringTable^[Counter] = nil then
         Inc(TotalCnt)
           else begin
                  {$IFNDEF MSDOS}
                    if NOT StringTable^[Counter]^.Alloced then
                      Inc(TotalCnt);
                  {$ENDIF}
                end; { else }
    end; { while }

  StrGetFreeCount := TotalCnt;
end; { func. StrGetFreeCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrAllocNew(var elx_Globals: elx_GlobalsType; var StrPtr: Longint; T: Longint): Boolean;
var Counter: Longint;
    BitCntr: Longint;
    Found  : Boolean;
begin
  {-- Initialize all variables ---------------------------------------------}
  Found := FALSE;

  {-- see if the last one freed is still available -------------------------}
  Counter := 0;
  with elx_Globals do
   while Counter < High(StrLastFreed) do
     begin
        if StrLastFreed[Counter] > 0 then
          begin
            {$IFNDEF MSDOS}
            if NOT StringTable^[StrLastFreed[Counter]]^.Alloced then
              begin
                Found := TRUE;
                StrPtr := StrLastFreed[Counter];
                BREAK;
              end; { if }
            {$ENDIF}

            StrLastFreed[Counter] := -1;
          end; { if }

       Inc(Counter);
     end; { while }

  {-- ok, our quick searches failed, start a full search -------------------}
  if NOT Found then
    begin
      with elx_Globals do
        begin
          {-- Get a new number ------------------------------------------------}
          Counter := StrLastIndex;
          if Counter = 0 then
            Counter := 1;
          Found := false;

          {-- first we try to find pre-allocated memory -----------------------}
          {$IFNDEF MSDOS}
          while (Counter < MaxStrings) AND (NOT Found) do
            begin
              if StringTable^[Counter] <> nil then
                if NOT StringTable^[Counter]^.Alloced then
                  begin
                    Found := true;
                    BREAK;
                  end; { if }

              Inc(Counter);

              {-- correct this one -----------------------------------------}
              {-- we try to start at the last allocated index, but if that -}
              {-- fails, we just try to start from the beginning -----------}
              if NOT Found then
                if Counter >= MaxStrings then
                 if StrLastIndex > 0 then
                   begin
                     Counter := 1;  // 0 = always nil, so dont use it
                     StrLastIndex := 0;
                   end; { if }
            end; { while }
          {$ENDIF}

          {-- and update the last string index --------------------------------}
          if Counter < (MaxStrings - 1) then
            StrLastIndex := Counter
              else StrLastIndex := 0;

          {-- if that fails, try something else -------------------------------}
          if NOT Found then
            begin
              Counter := 0;

              while (StringTable^[Counter] <> nil) AND (Counter < MaxStrings) do
                Inc(Counter);

              Found := (Counter <= MaxStrings);
            end; { if }

          {-- set the pointer -------------------------------------------------}
          StrPtr := Counter;
        end; { with }

      {-- It succeeded, allocate memory and come along ------------------------}
      if NOT Found then
        begin
          StrAllocNew := FALSE;
          EXIT;
        end {if }
          else StrAllocNew := TRUE;
    end; { if }

  {-- Actually allocate memory for it -------------------------------------}
  with elx_Globals do
    begin
      if StringTable^[StrPtr] = nil then
        begin
          GetMem(StringTable^[StrPtr], SizeOf(StrPtrRecord));
          FillChar(StringTable^[StrPtr]^, SizeOf(StrPtrRecord), #0);

{//showtime('No cached found...: ', StrPtr);}

          StringTable^[StrPtr]^.Len := T;

          {$IFNDEF MSDOS}
            StringTable^[StrPtr]^.Alloced := true;

            elx_Globals.StringTable^[StrPtr]^.AllocSz := StrGetNearSize(T);
            elx_Globals.StringTable^[StrPtr]^.Content := AllocMem(
                       elx_Globals.StringTable^[StrPtr]^.AllocSz);
          {$ENDIF}
        end
          else begin
                 {$IFNDEF MSDOS}
                   StringTable^[StrPtr]^.Alloced := true;
                   StringTable^[StrPtr]^.Len := T;

                   if T > StringTable^[StrPtr]^.AllocSz then
                     begin
                       StrGrowString(elx_Globals, StrPtr, T);
                     end; { if }
                 {$ENDIF}
               end; { else }

      StrAllocNew := Found;
    end; { with }
end; { func. StrAllocNew }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IntStr(N, Len: Longint): String;             { Convert int to str }
var Temp: String;
begin
  Str(N:Len, temp);
  IntStr := Temp;
end; { func. FStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GlobalInitialize(var elx_Globals: elx_GlobalsType): Boolean; { init vars used both by compiler and int }
var Counter: Longint;
begin
  with elx_Globals do
    begin
      {-- Initialize some variables ------------------------------------------}
      CurSrcFile := 0;
      ErrorFile := NIL;
      IdentTable := NIL;
      ArrayTable := NIL;
      {$IFNDEF MSDOS}
        DynArrayTable := NIL;
      {$ENDIF}
      BlockTable := NIL;
      StringTable := NIL;
      Display := NIL;
      BaseZeroArray := NIL;
      UserCallList := NIL;
      UserParamList := NIL;
      HighIdentCount := 0;
      CurCallList := 0;
      GlobalInitialize := FALSE;
      FillChar(StrLastFreed, SizeOf(StrLastFreed), #0);
      StrLastIndex := 0;
      BaseZeroIdx := 0;
      InitialStrCnt := 1;

      {-- Get some memory ----------------------------------------------------}
      New(IdentTable);
      New(ArrayTable);
      {$IFNDEF MSDOS}
        New(DynArrayTable);
      {$ENDIF}
      New(BlockTable);
      New(StringTable);
      New(Display);
      New(UserCallList);
      New(UserParamList);
      New(BaseZeroArray);

      {-- make sure all memory is allocated ----------------------------------}
      if (IdentTable = nil) OR (ArrayTable = nil) OR
          (BlockTable = nil) OR (StringTable = nil) OR (Display = nil) OR
           (UserCallList = nil) OR (UserParamList = nil)
             {$IFNDEF MSDOS} OR (DynArrayTable = NIL) {$ENDIF} OR
             (BaseZeroArray = nil) then
             begin
               if IdentTable <> nil then Dispose(IdentTable);
               if ArrayTable <> nil then Dispose(ArrayTable);
               {$IFNDEF MSDOS}
                 if DynArrayTable <> nil then Dispose(DynArrayTable);
               {$ENDIF}
               if BlockTable <> nil then Dispose(BlockTable);
               if StringTable <> nil then Dispose(StringTable);
               if Display <> nil then Dispose(Display);
               if UserCallList <> nil then Dispose(UserCallList);
               if UserParamList <> nil then Dispose(UserParamList);
               if BaseZeroArray <> nil then Dispose(BaseZeroArray);

               GlobalInitialize := FALSE;
               EXIT;
             end; { if }

      {-- Clear the string list ----------------------------------------------}
      FillChar(StringTable^, SizeOf(StringTable^), #0);
      for counter := 0 to MaxStrings do
          StringTable^[Counter] := nil;

      {-- Create the NULL string we use --------------------------------------}
      StrAllocNew(elx_Globals, NulString, 0);
      StrSetString(elx_Globals, NulString, '', 0);      { Should always be zero! }

      if NulString <> 0 then
        begin
          GlobalInitialize := FALSE;
          EXIT;
        end { if }
          else {$IFNDEF MSDOS}StringTable^[NulString]^.Alloced := true{$ENDIF};

      {-- Set the result -----------------------------------------------------}
      GlobalInitialize := TRUE;

      {-- Initialize all the user hooks to nil -------------------------------}
      elx_AddUserFuncsHook := nil;
      elx_UserFuncsHook := nil;
      elx_SystemFuncsHook := nil;
      elx_UserWriteHook := nil;
      elx_UserReadHook := nil;

      {-- Clear the parameter lists ------------------------------------------}
      for Counter := 0 to MaxParamList do
        UserParamList^[Counter] := nil;

      for Counter := 0 to MaxCallList do
        UserCallList^[Counter] := nil;

      for Counter := 1 to MaxArray do
        ArrayTable^[Counter] := nil;

      for Counter := 1 to MaxDynArray do
        DynArrayTable^[Counter].alloc_high := -1;
    end; { with }

end; { proc. GlobalInitialize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateDateTime: String;
{ '01-???-2000 00:00' }

const
  MonthStr: Array[1..12] of String[3] =
                ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

function LeadingZero(W: Word): String;
var Tmp: String;
begin
  Tmp := IntStr(W, 2);
  if Tmp[1] = #32 then Tmp[1] := '0';

  LeadingZero := Tmp;
end; { func. LeadingZero }

var Y,D,M,Dow: Word;
    Hour, Min, Sec, MSec: Word;
begin
  {$IFNDEF DELPHI}
    GetDate(Y,M,D,Dow);
    GetTime(Hour, Min, Sec, MSec);
  {$ENDIF}

  CreateDateTime := LeadingZero(D) + '-' +
                    MonthStr[M] + '-' +
                    IntStr(Y, 0) + '  ' +
                    LeadingZero(Hour) + ':' +
                    LeadingZero(Min);
end; { func. CreateDateTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function WriteModuleToDisk(var elx_Globals: elx_GlobalsType; FName: String): Boolean;      { Write to disk }
var ModFile: pFileObj;

procedure WriteDisplay;
var Module : elx_Module;
    Counter: Longint;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the display header to disk -----------------------------------}
  Module.ElxTyp := elx_Mod_Display;
  Module.NrRecs := MaxLevel;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 0 to Module.NrRecs do
    with elx_Globals do
      begin
        ModFile^.BlkWrite(Display^[Counter], SizeOf(Display^[Counter]));
      end; { for }
end; { proc. WriteDisplay }

procedure WriteIdentTable;
var Module : elx_Module;
    Counter: Longint;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the display header to disk -----------------------------------}
  Module.ElxTyp := elx_Mod_IdentTable;
  Module.NrRecs := elx_Globals.HighIdentCount;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 0 to Module.NrRecs do
    with elx_Globals do
      begin
        ModFile^.BlkWrite(IdentTable^[Counter], SizeOf(IdentTable^[Counter]));
      end; { for }
end; { proc. WriteIdentTable }

procedure WriteArrayTable;
var Module : elx_Module;
    Counter: Longint;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the display header to disk -----------------------------------}
  Module.ElxTyp := elx_Mod_ArrayTable;
  Module.NrRecs := elx_Globals.ArrayCount;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 1 to Module.NrRecs do
    with elx_Globals do
      begin
        ModFile^.BlkWrite(ArrayTable^[Counter]^.ArrayInf, SizeOf(ArrayTable^[Counter]^.ArrayInf));
      end; { for }
end; { proc. WriteArrayTable }

procedure WriteDynArrayTable;
var Module : elx_Module;
    Counter: Longint;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the display header to disk -----------------------------------}
  Module.ElxTyp := elx_Mod_DynArrayTable;
  Module.NrRecs := elx_Globals.DynArrayCount;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 1 to Module.NrRecs do
    with elx_Globals do
      begin
        {$IFNDEF MSDOS}
          ModFile^.BlkWrite(DynArrayTable^[Counter], SizeOf(DynArrayTable^[Counter]));
        {$ENDIF}
      end; { for }
end; { proc. WriteDynArrayTable }

procedure WriteBlockTable;
var Module : elx_Module;
    Counter: Longint;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the display header to disk -----------------------------------}
  Module.ElxTyp := elx_Mod_BlockTable;
  Module.NrRecs := elx_Globals.BlockCount;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 1 to Module.NrRecs do
    with elx_Globals do
      begin
        ModFile^.BlkWrite(BlockTable^[Counter], SizeOf(BlockTable^[Counter]));
      end; { for }
end; { proc. WriteBlockTable }

procedure WriteRealsTable;
var Module : elx_Module;
    Counter: Longint;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the display header to disk -----------------------------------}
  Module.ElxTyp := elx_Mod_RealsTable;
  Module.NrRecs := elx_Globals.RealCount2;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 1 to Module.NrRecs do
    with elx_Globals do
      begin
        ModFile^.BlkWrite(RealsTable[Counter], SizeOf(RealsTable[Counter]));
      end; { for }
end; { proc. WriteRealsTable }

procedure WriteStringsTable;
var Module   : elx_Module;
    StrRecord: elx_StrModule;
    Counter  : Longint;
    TmpCount : Longint;
    TmpBuf   : Array[0..16384] of char;
begin
  {-- Initialize some vars ------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);
  TmpCount := 0;

  {-- Get a new number ----------------------------------------------------}
  for Counter := 0 to MaxStrings do
    if elx_Globals.StringTable^[Counter] <> nil then
      begin
        Inc(TmpCount);
      end; { if }

  {-- Exit if no strings at all ------------------------------------------}
  if TmpCount = 0 then EXIT;

  {-- Write the display header to disk -----------------------------------}
  Module.ElxTyp := elx_Mod_StrTable;
  Module.NrRecs := TmpCount;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 0 to MaxStrings do
    begin
        if elx_Globals.StringTable^[Counter] <> nil then
          begin
            {-- make sure this table is null terminated ----------------}
            FillChar(TmpBuf, SizeOf(TmpBuf), #0);

            {-- Calculate the number and setup the header --------------}
            TmpCount := Counter;
            StrRecord.StrItem := TmpCount;

            {-- prepare the data to be written ------------------------}
            StrRecord.Size := elx_Globals.StringTable^[TmpCount]^.Len + 1;
            {$IFDEF MSDOS}
              Move(elx_globals.StringTable^[TmpCount]^.Content[1], TmpBuf[0],
                     SizeOf(elx_Globals.StringTable^[TmpCount]^));
            {$ELSE}
              StrCopy(TmpBuf, pChar(elx_globals.StringTable^[TmpCount]^.Content));
            {$ENDIF}

            {-- Write the header, and then the string ------------------}
            ModFile^.BlkWrite(StrRecord, SizeOf(StrRecord));
            ModFile^.BlkWrite(TmpBuf, StrRecord.Size);
          end; { if }
    end; { if }
end; { proc. WriteStringsTable }

procedure WriteCallListTable;
var Counter: Longint;
    Module : elx_Module;
    TmpStr : String;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the program code back to disk --------------------------------}
  Module.ElxTyp := elx_Mod_CallList;
  Module.NrRecs := elx_Globals.CurCallList;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now read the actual records from disk ------------------------------}
  if Module.NrRecs > 0 then
    for Counter := 0 to (Module.NrRecs) do
      begin
        if elx_GLobals.UserCallList^[Counter] <> nil then
          TmpStr := elx_Globals.UserCallList^[Counter]^
            else TmpStr := '';

        ModFile^.BlkWrite(TmpStr[0], Length(TmpStr) + 1);
      end; { for }
end; { proc. WriteCallListTable }

procedure WriteProgCode;
var Module : elx_Module;
    Counter: Longint;
begin
  {-- Clean out the vars -------------------------------------------------}
  FillChar(Module, SizeOf(elx_Module), #0);

  {-- Write the program code back to disk --------------------------------}
  Module.ElxTyp := elx_Mod_ProgCode;
  Module.NrRecs := elx_Globals.CurCode;

  {-- Exit if no records at all ------------------------------------------}
  If Module.NrRecs = 0 then EXIT;

  {-- Write module header ------------------------------------------------}
  ModFile^.BlkWrite(Module, SizeOf(Module));

  {-- Now write the actual records to disk -------------------------------}
  for Counter := 0 to Module.NrRecs do
    with elx_Globals do
      begin
        ModFile^.BlkWrite(ProgCode[Counter], SizeOf(ProgCode[Counter]));
      end; { for }
end; { proc. WriteProgCode }

begin
  {-- Initialize some variables ------------------------------------------}
  WriteModuleToDisk := FALSE;

  {-- Create the file ----------------------------------------------------}
  New(ModFile, Init);
  ModFile^.Assign(FName);
  if NOT ModFile^.Create(1) then
    begin
      Dispose(ModFile, Done);

      WriteModuleToDisk := FALSE;
      EXIT;
    end; { if }

  {-- Create the header --------------------------------------------------}
  FillChar(elx_Globals.ModHdr, SizeOf(elx_ModHdr), #0);

  elx_Globals.ModHdr.ElxInfo  := ElxInfoStr;
  elx_Globals.ModHdr.Version  := ElxVersionID;
  elx_Globals.ModHdr.ModName  := elx_Globals.ModuleName;
  elx_Globals.ModHdr.Datetime := CreateDateTime; { '01-???-2000 00:00'; }
  elx_Globals.ModHdr.StartBlck:= elx_Globals.StartBlock;

  {-- Write the header to disk -------------------------------------------}
  ModFile^.BlkWrite(elx_Globals.ModHdr, SizeOf(elx_Globals.ModHdr));

  {-- Write the display to disk ------------------------------------------}
  WriteDisplay;

  {-- Write the ident table to disk --------------------------------------}
  WriteIdentTable;

  {-- Write the array table to disk --------------------------------------}
  WriteArrayTable;

  {-- Write the string array table to disk -------------------------------}
  WriteDynArrayTable;

  {-- Write the block table to disk --------------------------------------}
  WriteBlockTable;

  {-- Write the reals table to disk --------------------------------------}
  WriteRealsTable;

  {-- Write the strings table to disk ------------------------------------}
  WriteStringsTable;

  {-- Write the user parameter lists to disk -----------------------------}
  WriteCallListTable;

  {-- Write the program code to disk -------------------------------------}
  WriteProgCode;

  {-- Close the module file ----------------------------------------------}
  Dispose(ModFile, Done);
  WriteModuleToDisk := TRUE;
end; { func. WriteModuleToDisk }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ReadModuleFromDisk(var elx_Globals: elx_GlobalsType; FName: String): Boolean;    { Read from disk }
var ModFile: pFileObj;
    Module : elx_Module;
    
    ick: array[0..100] of real;

procedure ReadDisplay(var Module: elx_Module);
begin
  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the actual records back from disk -------------------------}
  with elx_Globals do
    ModFile^.BlkRead(Display^[0], SizeOf(Display^[0]) * (Module.NrRecs + 1));
end; { proc. ReadDisplay }

procedure ReadIdentTable(var Module: elx_Module);
begin
  {-- Set identcount to the value as set by the module -------------------}
  elx_Globals.IdentCount := Module.NrRecs;

  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the records back from disk --------------------------------}
  with elx_Globals do
    ModFile^.BlkRead(IdentTable^[0], SizeOf(IdentTable^[0]) * (Module.NrRecs + 1));
end; { proc. ReadIdentTable }

procedure ReadArrayTable(var Module: elx_Module);
var Counter: Longint;
begin
  {-- Read the arraycount back from disk ---------------------------------}
  elx_Globals.ArrayCount := Module.NrRecs;

  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the actual records from disk ------------------------------}
  for Counter := 1 to elx_Globals.ArrayCount do
    with elx_Globals do
      begin
        New(ArrayTable^[Counter], Init);

        ModFile^.BlkRead(ArrayTable^[Counter]^.ArrayInf,
                         SizeOf(ArrayTable^[Counter]^.ArrayInf));
      end; { for }
end; { proc. ReadArrayTable }

procedure ReadDynArrayTable(var Module: elx_Module);
var Counter       : Longint;
    SaveAlloc_High: Longint; 
begin
  {-- Read the arraycount back from disk ---------------------------------}
  elx_Globals.DynArrayCount := Module.NrRecs;

  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the actual records from disk ------------------------------}
 {$IFNDEF MSDOS}
  for Counter := 1 to elx_Globals.DynArrayCount do
   with elx_Globals do
     if DynArrayTable^[Counter].alloc_high = -1 then
      begin
      	{-- read from disk -----------------------------------------------}
        ModFile^.BlkRead(DynArrayTable^[Counter],
                         SizeOf(DynArrayTable^[Counter]));
      	
      	{-- save wether this dynamic array has been initted or not -------}
        DynArrayInit(elx_Globals, Counter);
        DynArrayTable^[Counter].LowestItem := MaxLongInt;
      end { for }
        else ModFile^.Seek(ModFile^.FilePos + SizeOf(DynArrayTable^[Counter]));
 {$ENDIF}
end; { proc. ReadDynArrayTable }


procedure ReadBlockTable(var Module: elx_Module);
begin
  {-- Read the blockcount variable back from disk ------------------------}
  elx_Globals.BlockCount := Module.NrRecs;

  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the records back from disk --------------------------------}
  with elx_Globals do
    ModFile^.BlkRead(BlockTable^[1], SizeOf(BlockTable^[1]) * Module.NrRecs);
end; { proc. ReadBlockTable }

procedure ReadRealsTable(var Module: elx_Module);
begin
  {-- Read the display header from disk ----------------------------------}
  elx_Globals.RealCount2 := Module.NrRecs;

  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the records back from disk --------------------------------}
  with elx_Globals do
    ModFile^.BlkRead(RealsTable[1], SizeOf(RealsTable[1]) * Module.NrRecs);
end; { proc. ReadRealsTable }

procedure ReadStringsTable(var Module: elx_Module);
var Counter  : Longint;
    TmpCount : Longint;
    StrRecord: elx_StrModule;
    TmpBuf   : Array[0..16384] of Char;
    
    AllocNew : Boolean;
begin
  {-- Read the value back from disk --------------------------------------}
  TmpCount := Module.NrRecs;

  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the actual records from disk ------------------------------}
  for Counter := 1 to TmpCount do
    with elx_Globals do
      begin
        {-- First read the string item -----------------------------------}
        ModFile^.BlkRead(StrRecord, SizeOf(StrRecord));

        {-- Now allocate memory ------------------------------------------}
        if StrRecord.StrItem = 0 then
          StrFreeString(elx_GLobals, StrRecord.StrItem);

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{ this appears to leak memory, but i thin its }
{ the memory manager as all allocated strings }
{ are freed in fact }
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

	{-- first determine wether we want a new string memory -----------}
	if StringTable^[StrRecord.StrItem] <> nil then
	  AllocNew := false
	    else AllocNew := true;

        {-- clear memory, else ansistring get confused -------------------}
        if AllocNew then
	  GetMem(StringTable^[StrRecord.StrItem], SizeOf(StrPtrRecord));
	  
        FillChar(StringTable^[StrRecord.StrItem]^, SizeOf(StrPtrRecord), #0);

        {-- and read it --------------------------------------------------}
        StringTable^[StrRecord.StrItem]^.Len := StrRecord.Size - 1;
        {$IFNDEF MSDOS}
          StringTable^[StrRecord.StrItem]^.Alloced := true;
          StrGrowString(elx_Globals, StrRecord.StrItem, StrRecord.Size);
        {$ENDIF}

        ModFile^.BlkRead(TmpBuf[0], StrRecord.Size);
        TmpBuf[StrRecord.Size + 1] := #0;

        {$IFNDEF MSDOS}
          StrCopy(PChar(StringTable^[StrRecord.StrItem]^.Content), TmpBuf);
        {$ELSE}
          Move(TmpBuf, StringTable^[StrRecord.StrItem]^.Content[1], StrRecord.Size);
          StringTable^[StrRecord.StrItem]^.Content[0] := Chr(StrRecord.Size - 1);
        {$ENDIF}

        elx_Globals.InitialStrCnt := StrRecord.StrItem;
      end; { for }
end; { proc. ReadStringsTable }

procedure ReadProgCode(var Module: elx_Module);
begin
  {-- Read the program code back from disk -------------------------------}
  elx_Globals.CurCode := Module.NrRecs;

  {-- Exit if nothing to read --------------------------------------------}
  if Module.NrRecs = 0 then EXIT;

  {-- Now read the actual records from disk ------------------------------}
  with elx_Globals do
    ModFile^.BlkRead(ProgCode[0], SizeOf(ProgCode[0]) * (Module.NrRecs + 1));
end; { proc. ReadProgCode }


procedure ReadCallListTable(var Module: elx_Module);
var Counter: Longint;
    TmpStr : String;
begin
  {-- Now read the actual records from disk ------------------------------}
  if Module.NrRecs = 0 then EXIT;

  for Counter := 0 to (Module.NrRecs) do
    with elx_Globals do
      begin
        ModFile^.BlkRead(TmpStr[0], 1);
        if Ord(TmpStr[0]) > 0 then
          ModFile^.BlkRead(TmpStr[1], Ord(TmpStr[0]));

        GetMem(UserCallList^[Counter], Length(TmpStr) + 1);
        UserCallList^[Counter]^ := TmpStr;

        CurCallList := Counter;
      end; { for }

  with elx_Globals do
    if CurCallList > 0 then
      Inc(CurCallList);
end; { proc. ReadCallListTable }


begin
{$IFDEF ELX_PROFILE}
{!!!} ick[0] := GetMicroSeconds;	
{$ENDIF}

  {-- Initialize some vars -----------------------------------------------}
  ReadModuleFromDisk := FALSE;

  {-- Create the file ----------------------------------------------------}
  New(ModFile, Init);
  ModFile^.Assign(FName);
  if NOT ModFile^.Open(1) then
    begin
      ReadModuleFromDisk := FALSE;
      Dispose(ModFile, Done);

      EXIT;
    end; { if }

{$IFDEF ELX_PROFILE}
{!!!} ick[1] := GetMicroSeconds;	
{$ENDIF}

  {-- Read the header from disk ------------------------------------------}
  ModFile^.BlkRead(elx_Globals.ModHdr, SizeOf(elx_Globals.ModHdr));

{$IFDEF ELX_PROFILE}
{!!!} ick[3] := GetMicroSeconds;	
{$ENDIF}

  {-- Set necessary variables --------------------------------------------}
  elx_Globals.ModuleName := elx_Globals.ModHdr.ModName;
  elx_Globals.StartBlock := elx_Globals.ModHdr.StartBlck;

{$IFDEF ELX_PROFILE}
{!!!} ick[4] := GetMicroSeconds;	
writeln;writeln;writeln;
{$ENDIF}

  {-- Now loop through all modules, and process them ---------------------}
  REPEAT
{$IFDEF ELX_PROFILE}
{!!!} ick[5] := GetMicroSeconds;	
{$ENDIF}

    {-- Read the module header -------------------------------------------}
    ModFile^.BlkRead(Module, SizeOf(Module));

    Case Module.ElxTyp of
      elx_Mod_Display      : ReadDisplay(Module);
      elx_Mod_IdentTable   : ReadIdentTable(Module);
      elx_Mod_ArrayTable   : ReadArrayTable(Module);
      elx_Mod_BlockTable   : ReadBlockTable(Module);
      elx_Mod_RealsTable   : ReadRealsTable(Module);
      elx_Mod_StrTable     : ReadStringsTable(Module);
      elx_Mod_CallList     : ReadCallListTable(Module);
      elx_Mod_DynArrayTable: ReadDynArrayTable(Module);
      elx_Mod_ProgCode     : ReadProgCode(Module);
        else begin
               ReadModuleFromDisk := FALSE;
               EXIT;
             end; { else }
    end; { case }
{$IFDEF ELX_PROFILE}
{!!!} ick[6] := GetMicroSeconds;
writeln('Difference after: ', Module.ElxTyp, ' == : ', real(ick[6] - ick[5]):3:6, ' <br />');
{$ENDIF}

  UNTIL ModFile^.EOF;

{$IFDEF ELX_PROFILE}
{!!!} ick[7] := GetMicroSeconds;	
{$ENDIF}

  {-- Close the module file ----------------------------------------------}
  Dispose(ModFile, Done);
  ReadModuleFromDisk := TRUE;
end; { func. ReadModuleFromDisk }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OpCodeToStr(const OpCode: Longint): String;
begin
  Case OpCode of
    op_LoadAddress   : OpCodeToStr := 'Load address';
    op_LoadValue     : OpCodeToStr := 'Load value';
    op_LoadIndirect  : OpCodeToStr := 'Load indirect';
    op_UpdateDisplay : OpCodeToStr := 'Update display[]';
    op_IntError_04   : OpCodeToStr := 'Internal error (04)';
    op_IntError_05   : OpCodeToStr := 'Internal error (05)';
    op_IntError_06   : OpCodeToStr := 'Internal error (06)';
    op_Concatenate   : OpCodeToStr := 'Concatenation';
    op_RunFunction   : OpCodeToStr := 'Run a function';
    op_OffSet        : OpCodeToStr := 'Offset';
    op_JumpTo        : OpCodeToStr := 'Jump to address';
    op_ConditionalJmp: OpCodeToStr := 'Conditional jump';
    op_Switch        : OpCodeToStr := 'Switch';
    op_IntError_13   : OpCodeToStr := 'Internal error (13)';
    op_For1Up        : OpCodeToStr := 'For up (begin) - run only twice';
    op_For2Up        : OpCodeToStr := 'For up (end) - run as many times as needed';
    op_For1Down      : OpCodeToStr := 'For 1 down';
    op_For2Down      : OpCodeToStr := 'For 2 down';
    op_MarkStack     : OpCodeToStr := 'Mark stack';
    op_Call          : OpCodeToStr := 'Call';
    op_Index1        : OpCodeToStr := 'Index1';
    op_Index         : OpCodeToStr := 'Index';
    op_LoadBlock     : OpCodeToStr := 'Load block';
    op_CopyBlock     : OpCodeToStr := 'Copy block';
    op_Literal       : OpCodeToStr := 'Literal';
    op_LoadReal      : OpCodeToStr := 'Load real';
    op_LoadFloat     : OpCodeToStr := 'Load float';
    op_Read          : OpCodeToStr := 'Read';
    op_IntError_28   : OpCodeToStr := 'Internal error (28)';
    op_Write1        : OpCodeToStr := 'Write1 is (write)';
    op_Write2        : OpCodeToStr := 'Write2 is (write with width specifier)';
    op_CharAndString : OpCodeToStr := 'Char + String';
    op_StringRel     : OpCodeToStr := 'String relations';
    op_RunUserFunc   : OpCodeToStr := 'Run user func';
    op_StringLink    : OpCodeToStr := 'Linkup string to freeing table';
    op_DynArIndex    : OpCodeToStr := 'DynArIndex';
    op_DynArCommand  : OpCodeToStr := 'DynArCommand';
    op_DynArChild    : OpCodeToStr := 'DynArChild';
    op_IntError_38   : OpCodeToStr := 'Internal error (38)';
    op_IntError_39   : OpCodeToStr := 'Internal error (39)';
    op_IntError_40   : OpCodeToStr := 'Internal error (40)';
    op_IntError_41   : OpCodeToStr := 'Internal error (41)';
    op_IntError_42   : OpCodeToStr := 'Internal error (42)';
    op_IntError_43   : OpCodeToStr := 'Internal error (43)';
    op_IntError_44   : OpCodeToStr := 'Internal error (44)';
    op_IntError_45   : OpCodeToStr := 'Internal error (45)';
    op_IntError_46   : OpCodeToStr := 'Internal error (46)';
    op_IntError_47   : OpCodeToStr := 'Internal error (47)';
    op_IntError_48   : OpCodeToStr := 'Internal error (48)';
    op_IntError_49   : OpCodeToStr := 'Internal error (49)';
    op_IntError_50   : OpCodeToStr := 'Internal error (50)';
    op_IntError_51   : OpCodeToStr := 'Internal error (51)';
    op_IntError_52   : OpCodeToStr := 'Internal error (52)';
    op_IntError_53   : OpCodeToStr := 'Internal error (53)';
    op_IntError_54   : OpCodeToStr := 'Internal error (54)';
    op_IntError_55   : OpCodeToStr := 'Internal error (55)';
    op_IntError_56   : OpCodeToStr := 'Internal error (56)';
    op_IntError_57   : OpCodeToStr := 'Internal error (57)';
    op_IntError_58   : OpCodeToStr := 'Internal error (58)';
    op_IntError_59   : OpCodeToStr := 'Internal error (59)';
    op_IntError_60   : OpCodeToStr := 'Internal error (60)';
    op_IntError_61   : OpCodeToStr := 'Internal error (61)';
    op_IntError_62   : OpCodeToStr := 'Internal error (62)';
    op_IntError_63   : OpCodeToStr := 'Internal error (63)';
    op_IntError_64   : OpCodeToStr := 'Internal error (64)';
    op_IntError_65   : OpCodeToStr := 'Internal error (65)';
    op_IntError_66   : OpCodeToStr := 'Internal error (66)';
    op_IntError_67   : OpCodeToStr := 'Internal error (67)';
    op_IntError_68   : OpCodeToStr := 'Internal error (68)';
    op_IntError_69   : OpCodeToStr := 'Internal error (69)';
    op_IntError_70   : OpCodeToStr := 'Internal error (70)';
    op_IntError_71   : OpCodeToStr := 'Internal error (71)';
    op_IntError_72   : OpCodeToStr := 'Internal error (72)';
    op_IntError_73   : OpCodeToStr := 'Internal error (73)';
    op_IntError_74   : OpCodeToStr := 'Internal error (74)';
    op_IntError_75   : OpCodeToStr := 'Internal error (75)';
    op_IntError_76   : OpCodeToStr := 'Internal error (76)';
    op_IntError_77   : OpCodeToStr := 'Internal error (77)';
    op_IntError_78   : OpCodeToStr := 'Internal error (78)';
    op_IntError_79   : OpCodeToStr := 'Internal error (79)';
    op_IntError_80   : OpCodeToStr := 'Internal error (80)';
    op_IntError_81   : OpCodeToStr := 'Internal error (81)';
    op_IntError_82   : OpCodeToStr := 'Internal error (82)';
    op_IntError_83   : OpCodeToStr := 'Internal error (83)';
    op_IntError_84   : OpCodeToStr := 'Internal error (84)';
    op_IntError_85   : OpCodeToStr := 'Internal error (85)';
    op_IntError_86   : OpCodeToStr := 'Internal error (86)';
    op_IntError_87   : OpCodeToStr := 'Internal error (87)';
    op_IntError_88   : OpCodeToStr := 'Internal error (88)';
    op_IntError_89   : OpCodeToStr := 'Internal error (89)';
    op_IntError_90   : OpCodeToStr := 'Internal error (90)';
    op_IntError_91   : OpCodeToStr := 'Internal error (91)';
    op_IntError_92   : OpCodeToStr := 'Internal error (92)';
    op_IntError_93   : OpCodeToStr := 'Internal error (93)';
    op_IntError_94   : OpCodeToStr := 'Internal error (94)';
    op_IntError_95   : OpCodeToStr := 'Internal error (95)';
    op_IntError_96   : OpCodeToStr := 'Internal error (96)';
    op_IntError_97   : OpCodeToStr := 'Internal error (97)';
    op_IntError_98   : OpCodeToStr := 'Internal error (98)';
    op_IntError_99   : OpCodeToStr := 'Internal error (99)';
    op_IntError_100  : OpCodeToStr := 'Internal error (100)';
    op_IntError_101  : OpCodeToStr := 'Internal error (101)';
    op_IntError_102  : OpCodeToStr := 'Internal error (102)';
    op_IntError_103  : OpCodeToStr := 'Internal error (103)';
    op_IntError_104  : OpCodeToStr := 'Internal error (104)';
    op_IntError_105  : OpCodeToStr := 'Internal error (105)';
    op_IntError_106  : OpCodeToStr := 'Internal error (106)';
    op_IntError_107  : OpCodeToStr := 'Internal error (107)';
    op_IntError_108  : OpCodeToStr := 'Internal error (108)';
    op_IntError_109  : OpCodeToStr := 'Internal error (109)';
    op_IntError_110  : OpCodeToStr := 'Internal error (110)';
    op_IntError_111  : OpCodeToStr := 'Internal error (111)';
    op_IntError_112  : OpCodeToStr := 'Internal error (112)';
    op_IntError_113  : OpCodeToStr := 'Internal error (113)';
    op_IntError_114  : OpCodeToStr := 'Internal error (114)';
    op_IntError_115  : OpCodeToStr := 'Internal error (115)';
    op_IntError_116  : OpCodeToStr := 'Internal error (116)';
    op_IntError_117  : OpCodeToStr := 'Internal error (117)';
    op_IntError_118  : OpCodeToStr := 'Internal error (118)';
    op_IntError_119  : OpCodeToStr := 'Internal error (119)';
    op_IntError_120  : OpCodeToStr := 'Internal error (110)';
    op_IntError_121  : OpCodeToStr := 'Internal error (121)';
    op_IntError_122  : OpCodeToStr := 'Internal error (122)';
    op_IntError_123  : OpCodeToStr := 'Internal error (123)';
    op_IntError_124  : OpCodeToStr := 'Internal error (124)';
    op_IntError_125  : OpCodeToStr := 'Internal error (125)';
    op_IntError_126  : OpCodeToStr := 'Internal error (126)';
    op_IntError_127  : OpCodeToStr := 'Internal error (127)';
    op_IntError_128  : OpCodeToStr := 'Internal error (128)';
    op_IntError_129  : OpCodeToStr := 'Internal error (129)';
    op_IntError_130  : OpCodeToStr := 'Internal error (130)';
    op_Halt          : OpCodeToStr := 'Halt';
    op_ExitProc      : OpCodeToStr := 'Exit procedure';
    op_ExitFunc      : OpCodeToStr := 'Exit function';
    op_CopyArray     : OpCodeToStr := 'Ar[1] := Ar[2]';
    op_MakeBoolNot   : OpCodeToStr := 'B := NOT B;';
    op_MakeIntNegative:  OpCodeToStr := '-I;';
    op_DecimalReal   : OpCodeToStr := 'WriteLn(R:xx:2)';
    op_Store         : OpCodeToStr := 'Store';
    op_RealEql       : OpCodeToStr := 'R := R';
    op_RealNeq       : OpCodeToStr := 'R <> R';
    op_RealLss       : OpCodeToStr := 'R < R';
    op_RealLeq       : OpCodeToStr := 'R <= R';
    op_RealGtr       : OpCodeToStr := 'R > R';
    op_RealGeq       : OpCodeToStr := 'R >= R';
    op_IntEql        : OpCodeToStr := 'I := I';
    op_IntNeq        : OpCodeToStr := 'I <> I';
    op_IntLss        : OpCodeToStr := 'I < I';
    op_IntLeq        : OpCodeToStr := 'I <= I';
    op_IntGtr        : OpCodeToStr := 'I > I';
    op_IntGeq        : OpCodeToStr := 'I >= I';
    op_BoolOr        : OpCodeToStr := 'B OR B';
    op_MinInt        : OpCodeToStr := 'I1 := I1 - I2';
    op_AddInt        : OpCodeToStr := 'I1 := I1 + I2';
    op_MinReal       : OpCodeToStr := 'R1 := R1 - R2';
    op_AddReal       : OpCodeToStr := 'R1 := R1 + R2';
    op_MakeBoolAnd   : OpCodeToStr := 'B := B AND B;';
    op_MultiplyInt   : OpCodeToStr := 'I := I * I;';
    op_DivideInt     : OpCodeToStr := 'I := I DIV I;';
    op_ModInt        : OpCodeToStr := 'I := I MOD I;';
    op_MultiplyReal  : OpCodeToStr := 'R := R * R';
    op_DivideReal    : OpCodeToStr := 'R := R / R';
    op_ReadLn        : OpCodeToStr := 'ReadLn';
    op_LineFeed      : OpCodeToStr := 'Linefeed (the "ln" in Writeln)';
    op_MakeRealNegative: OpCodeToStr := '-R;';
    op_StrIndex      : OpCodeToStr := 'String index (S[xx])';
    op_StringTemp    : OpCodeToStr := 'S := "Aa" + "Bb"';
    op_ArToString    : OpCodeToStr := 'Convert Array to string';
    op_StringAndChar : OpCodeToStr := 'S := C';
    op_StrAndStr     : OpCodeToStr := 'S := S';
    op_WriteStr      : OpCodeToStr := 'Write(s)';
    op_WriteStrTmp   : OpCodeToStr := 'Write("aa")';
    op_StrValueParam : OpCodeToStr := 'String as a value, parameter in func/proc';
    op_StrValParamTmp: OpCodeToStr := 'String as a value (temp), param. in func/proc';
    op_CharArAndStr  : OpCodeToStr := 'CharArray := String';
    op_CharArAndStrTmp: OpCodeToStr := 'CharArray := "String"';
    op_WriteStr2     : OpCodeToStr := 'Write2 is (writeln(s:16);)';
    op_WriteStr2Tmp  : OpCodeToStr := 'Write2 is (writeln("Maarten":16);)';
    op_IntError_178  : OpCodeToStr := 'Internal error (178)';
    op_IntError_179  : OpCodeToStr := 'Internal error (179)';
    op_IntError_180  : OpCodeToStr := 'Internal error (180)';
    op_IntError_181  : OpCodeToStr := 'Internal error (181)';
    op_IntError_182  : OpCodeToStr := 'Internal error (182)';
    op_IntError_183  : OpCodeToStr := 'Internal error (183)';
    op_IntError_184  : OpCodeToStr := 'Internal error (184)';
    op_IntError_185  : OpCodeToStr := 'Internal error (185)';
    op_IntError_186  : OpCodeToStr := 'Internal error (186)';
    op_IntError_187  : OpCodeToStr := 'Internal error (187)';
    op_IntError_188  : OpCodeToStr := 'Internal error (188)';
    op_IntError_189  : OpCodeToStr := 'Internal error (189)';
    op_IntError_190  : OpCodeToStr := 'Internal error (190)';
    op_IntError_191  : OpCodeToStr := 'Internal error (191)';
    op_IntError_192  : OpCodeToStr := 'Internal error (192)';
    op_IntError_193  : OpCodeToStr := 'Internal error (193)';
    op_IntError_194  : OpCodeToStr := 'Internal error (194)';
    op_IntError_195  : OpCodeToStr := 'Internal error (195)';
    op_IntError_196  : OpCodeToStr := 'Internal error (196)';
    op_IntError_197  : OpCodeToStr := 'Internal error (197)';
    op_IntError_198  : OpCodeToStr := 'Internal error (198)';
    op_IntError_199  : OpCodeToStr := 'Internal error (199)';
    op_IntError_200  : OpCodeToStr := 'Internal error (200)';
    op_IntError_201  : OpCodeToStr := 'Internal error (201)';
    op_IntError_202  : OpCodeToStr := 'Internal error (202)';
    op_IntError_203  : OpCodeToStr := 'Internal error (203)';
    op_IntError_204  : OpCodeToStr := 'Internal error (204)';
    op_IntError_205  : OpCodeToStr := 'Internal error (205)';
    op_IntError_206  : OpCodeToStr := 'Internal error (206)';
    op_IntError_207  : OpCodeToStr := 'Internal error (207)';
    op_IntError_208  : OpCodeToStr := 'Internal error (208)';
    op_IntError_209  : OpCodeToStr := 'Internal error (209)';
    op_IntError_210  : OpCodeToStr := 'Internal error (210)';
    op_IntError_211  : OpCodeToStr := 'Internal error (211)';
    op_IntError_212  : OpCodeToStr := 'Internal error (212)';
    op_IntError_213  : OpCodeToStr := 'Internal error (213)';
    op_IntError_214  : OpCodeToStr := 'Internal error (214)';
    op_IntError_215  : OpCodeToStr := 'Internal error (215)';
    op_IntError_216  : OpCodeToStr := 'Internal error (216)';
    op_IntError_217  : OpCodeToStr := 'Internal error (217)';
    op_IntError_218  : OpCodeToStr := 'Internal error (218)';
    op_IntError_219  : OpCodeToStr := 'Internal error (219)';
    op_IntError_220  : OpCodeToStr := 'Internal error (220)';
    op_IntError_221  : OpCodeToStr := 'Internal error (221)';
    op_IntError_222  : OpCodeToStr := 'Internal error (222)';
    op_IntError_223  : OpCodeToStr := 'Internal error (223)';
    op_IntError_224  : OpCodeToStr := 'Internal error (224)';
    op_IntError_225  : OpCodeToStr := 'Internal error (225)';
    op_IntError_226  : OpCodeToStr := 'Internal error (226)';
    op_IntError_227  : OpCodeToStr := 'Internal error (227)';
    op_IntError_228  : OpCodeToStr := 'Internal error (228)';
    op_IntError_229  : OpCodeToStr := 'Internal error (229)';
    op_IntError_230  : OpCodeToStr := 'Internal error (230)';
    op_IntError_231  : OpCodeToStr := 'Internal error (231)';
    op_IntError_232  : OpCodeToStr := 'Internal error (232)';
    op_IntError_233  : OpCodeToStr := 'Internal error (233)';
    op_IntError_234  : OpCodeToStr := 'Internal error (234)';
    op_IntError_235  : OpCodeToStr := 'Internal error (235)';
    op_IntError_236  : OpCodeToStr := 'Internal error (236)';
    op_IntError_237  : OpCodeToStr := 'Internal error (237)';
    op_IntError_238  : OpCodeToStr := 'Internal error (238)';
    op_IntError_239  : OpCodeToStr := 'Internal error (239)';
    op_IntError_240  : OpCodeToStr := 'Internal error (240)';
    op_IntError_241  : OpCodeToStr := 'Internal error (241)';
    op_IntError_242  : OpCodeToStr := 'Internal error (242)';
    op_IntError_243  : OpCodeToStr := 'Internal error (243)';
    op_IntError_244  : OpCodeToStr := 'Internal error (244)';
    op_IntError_245  : OpCodeToStr := 'Internal error (245)';
    op_IntError_246  : OpCodeToStr := 'Internal error (246)';
    op_IntError_247  : OpCodeToStr := 'Internal error (247)';
    op_IntError_248  : OpCodeToStr := 'Internal error (248)';
    op_IntError_249  : OpCodeToStr := 'Internal error (249)';
    op_IntError_250  : OpCodeToStr := 'Internal error (250)';
    op_IntError_251  : OpCodeToStr := 'Internal error (251)';
    op_IntError_252  : OpCodeToStr := 'Internal error (252)';
    op_IntError_253  : OpCodeToStr := 'Internal error (253)';
    op_IntError_254  : OpCodeToStr := 'Internal error (254)';
    op_IntError_255  : OpCodeToStr := 'Internal error (255)';
  end; { case }
end; { func. OpCodeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function HasErrors(var elx_Globals: elx_GlobalsType): Boolean;
var Counter: Longint;
begin
  HasErrors := FALSE;
  if elx_Globals.AbortCompile then
    begin
      HasErrors := true;
      EXIT;
   end; { if }

  with elx_Globals do
    for Counter := 0 to High(ErrorSet) do
      begin
        if ErrorSet[Counter] <> 0 then
          begin
            HasErrors := TRUE;
            EXIT;
          end; { if }
      end; { if }
end; { func. hasErrors }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GlobalFreeMemory(var elx_Globals: elx_GlobalsType; EndExit, FreeModule: Boolean);
var Counter: Longint;
    BitCntr: Longint;
    TmpItem: Longint;
begin
  with elx_Globals do
    begin
      {-- Dispose generic memory ---------------------------------------------}
      if EndExit then
       if IdentTable <> nil then
        Dispose(IdentTable);

      if EndExit then
       if ArrayTable <> nil then
         begin
           for Counter := 1 to MaxArray do
             if ArrayTable^[Counter] <> nil then
               Dispose(ArrayTable^[Counter], Done);

           Dispose(ArrayTable);
         end; { if }

     {$IFNDEF MSDOS}
        if NOT FreeModule then
          begin
            if DynArrayTable <> nil then
              begin
                for Counter := 1 to MaxDynArray do
                  begin
                    DynArrayFreeIt(elx_Globals, Counter, false);
                  end; { if }
              end; { if }
          end; { if }

        if EndExit then
          if DynArrayTable <> nil then
            begin
              for Counter := 1 to MaxDynArray do
                begin
                    // DynArrayFreeIt(elx_Globals, Counter, true);
                end; { if }

              Dispose(DynArrayTable);
              DynArrayTable := nil;
            end; { if }
     {$ENDIF}

      if EndExit then
       if BlockTable <> nil then
        Dispose(BlockTable);

      if EndExit then
       if Display <> nil then
        Dispose(Display);

      if EndExit then
       if BaseZeroArray <> nil then
        Dispose(BaseZeroArray);

      {-- Dispose of strings -------------------------------------------------}
      if NOT FreeModule then
        begin
          {-- only dispose of the strings we didnt use -----------------------}
          for Counter := InitialStrCnt + 1 to MaxStrings do
            if StringTable^[Counter] <> nil then
              begin
                {$IFNDEF MSDOS}
                  StringTable^[Counter]^.Alloced := false;
                  PChar(elx_Globals.StringTable^[Counter]^.Content)[0] := #0;
                {$ENDIF}
                elx_Globals.StringTable^[Counter]^.Len := 0;
              end; { if }
        end; { if }

      {-- Dispose of strings -------------------------------------------------}
      if FreeModule then
       if StringTable <> nil then
       for Counter := 01 to (MaxStrings DIV 32) do
        begin
          for BitCntr := 0 to 31 do
            if StringTable^[Counter] <> nil then
              begin
                {-- Free the string ----------------------------------------------}
                TmpItem := ((Counter - 1) * 32) + BitCntr;
                StrReleaseString(elx_Globals, TmpItem);
              end; { for }
        end; { for }

      {-- Dispose of the nulstring -------------------------------------------}
      if EndExit then
        begin
          StrReleaseString(elx_Globals, NulString);
        end; { if }


      {-- Free up the total string table -------------------------------------}
      if EndExit then
       if StringTable <> nil then
        Dispose(StringTable);

      {-- Dispose of parameter lists -----------------------------------------}
      if EndExit then
       if UserParamList <> nil then
        begin
          for Counter := 0 to MaxParamList do
            if UserParamList^[Counter] <> nil then
              begin
                FreeMem(UserParamList^[Counter], Length(UserParamList^[Counter]^) + 1);
                UserParamList^[Counter] := nil;
              end; { if }
        end; { if }

      if EndExit then
       if UserParamList <> nil then
        Dispose(UserParamList);

      {-- Dispose of userfunc call list --------------------------------------}
      if FreeModule then
       if CurCallList > 0 then
        if UserCallList <> nil then
          begin
            for Counter := 0 to (CurCallList - 1) do
              begin
                FreeMem(UserCallList^[Counter], Length(UserCallList^[Counter]^) + 1);
              end; { for }

            CurCallList := 0;
          end; { if }

      if EndExit then
       if UserCallList <> nil then
        Dispose(UserCallList);
       
    end; { with }

end; { proc. GlobalFreeMemory }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetFieldSize(var elx_Globals: elx_GlobalsType; var Ident: IdentRecord): Longint;
begin
  GetFieldSize := 0;

  if Ident.MaxSize >= 0 then
   begin
     GetFieldSize := Ident.MaxSize;

     if Ident.Typ = typ_Strngs then
       GetFieldSize := Ident.MaxSize + 1;
   end
      else begin
             case Ident.typ of
               typ_ints     : GetFieldSize := 2;
               typ_Reals    : GetFieldSize := SizeOf(Real);
               typ_Bools    : GetFieldSize := SizeOf(Boolean);
               typ_Chars    : GetFieldSize := SizeOf(Char);
               typ_Strngs   : GetFieldSize := 255 + 1;
               typ_arrays   : begin
                                With elx_Globals, ArrayTable^[Ident.Ref].ArrayInf do
                                  GetFieldSize := (High - Low + 1) * ElementMaxSize;
                              end; { typ_Arrays }
               typ_NoTyp    : begin
                                GetFieldSize := elx_Globals.BlockTable^[Ident.Ref].ByteSize;
                              end; { if }
               typ_Records  : GetFieldSize := elx_Globals.BlockTable^[Ident.Ref].ByteSize;
               typ_DynArray : GetFieldSize := 1;
             end; { case }
           end; { else }
end; { func. GetFieldSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elx_AddUserFuncs(var elx_GlobalBuf; OnlyParamList: Boolean);
var SaveOnlyParamList: Boolean;
    elx_Globals      : elx_GlobalsType ABSOLUTE elx_GlobalBuf;

procedure PartOne;
begin
  AddUserIdent(elx_Globals, '                              ', obj_procedure, typ_notyp,   0, '');
  AddUserIdent(elx_Globals, 'insert                        ', obj_procedure, typ_notyp,   1, 'sSi');
  AddUserIdent(elx_Globals, 'delete                        ', obj_procedure, typ_notyp,   2, 'Sii');
  AddUserIdent(elx_Globals, 'clearkeybuffer                ', obj_procedure, typ_notyp,   3, '');
  AddUserIdent(elx_Globals, 'waitforenter                  ', obj_procedure, typ_notyp,   4, 'bb');
  AddUserIdent(elx_Globals, 'putinkeybuffer                ', obj_procedure, typ_notyp,   5, 'sb');
  AddUserIdent(elx_Globals, 'askyesno                      ', obj_function,  typ_bools,   6, 'b');
  AddUserIdent(elx_Globals, 'updatelocalscreen             ', obj_procedure, typ_notyp,   7, '');
  AddUserIdent(elx_Globals, 'rangeedit                     ', obj_procedure, typ_notyp,   8, 'Ssiii');
  AddUserIdent(elx_Globals, 'stringedit                    ', obj_procedure, typ_notyp,   9, 'Siibbb');
  AddUserIdent(elx_Globals, 'resetmoreprompt               ', obj_procedure, typ_notyp,  10, '');
  AddUserIdent(elx_Globals, 'setstatusbar                  ', obj_procedure, typ_notyp,  11, 'i');
  AddUserIdent(elx_Globals, 'pushsysopkey                  ', obj_procedure, typ_notyp,  12, 'c');
  AddUserIdent(elx_Globals, 'runmenucmd                    ', obj_procedure, typ_notyp,  13, 'is');
  AddUserIdent(elx_Globals, 'getbbsversion                 ', obj_function,  typ_strngs, 14, '');
  AddUserIdent(elx_Globals, 'getenvironment                ', obj_function,  typ_strngs, 15, 's');
  AddUserIdent(elx_Globals, 'setuserflags                  ', obj_procedure, typ_notyp,  16, 'Es');
  AddUserIdent(elx_Globals, 'toggleuserflags               ', obj_procedure, typ_notyp,  17, 'Es');
  AddUserIdent(elx_Globals, 'resetuserflags                ', obj_procedure, typ_notyp,  18, 'Es');
  AddUserIdent(elx_Globals, 'checkuserflags                ', obj_function,  typ_bools,  19, 'Es');
  AddUserIdent(elx_Globals, 'cmpuserflags                  ', obj_function,  typ_bools,  20, 'EE');
  AddUserIdent(elx_Globals, 'ralget                        ', obj_function,  typ_strngs, 21, 'i');
  AddUserIdent(elx_Globals, 'ralgetkeys                    ', obj_function,  typ_strngs, 22, 'i');
  AddUserIdent(elx_Globals, 'ralgetstr                     ', obj_function,  typ_strngs, 23, 'i');
  AddUserIdent(elx_Globals, 'ralgetdsp                     ', obj_function,  typ_strngs, 24, 'i');
  AddUserIdent(elx_Globals, 'ralgetdefaultkey              ', obj_function,  typ_strngs, 25, 'i');
  AddUserIdent(elx_Globals, 'ralgettitleinfo               ', obj_function,  typ_strngs, 26, '');
  AddUserIdent(elx_Globals, 'ralgetkey                     ', obj_function,  typ_chars,  27, 'i');
  AddUserIdent(elx_Globals, 'ralhaskey                     ', obj_function,  typ_bools,  28, 'i');
  AddUserIdent(elx_Globals, 'ralgetcolor                   ', obj_function,  typ_bools,  29, 'i');
  AddUserIdent(elx_Globals, 'ralgetentries                 ', obj_function,  typ_ints,   30, '');
  AddUserIdent(elx_Globals, 'ralresetcache                 ', obj_procedure, typ_notyp,  31, '');
  AddUserIdent(elx_Globals, 'ralgetall                     ', obj_procedure, typ_notyp,  32, 'iBSSCBB');
  AddUserIdent(elx_Globals, 'setbit                        ', obj_procedure, typ_notyp,  33, 'Ii');
  AddUserIdent(elx_Globals, 'clearbit                      ', obj_procedure, typ_notyp,  34, 'Ii');
  AddUserIdent(elx_Globals, 'readbit                       ', obj_function,  typ_bools,  35, 'ii');
  AddUserIdent(elx_Globals, 'opentextfile                  ', obj_function,  typ_strngs, 36, 's');
  AddUserIdent(elx_Globals, 'displayhotfile                ', obj_function,  typ_chars,  37, 'ss');
  AddUserIdent(elx_Globals, 'displaywithcr                 ', obj_procedure, typ_notyp,  38, 's');
  AddUserIdent(elx_Globals, 'retrieveexitinfo              ', obj_procedure, typ_notyp,  39, 'e');
  AddUserIdent(elx_Globals, 'putexitinfo                   ', obj_procedure, typ_notyp,  40, 'e');
  AddUserIdent(elx_Globals, 'searchcombined                ', obj_function,  typ_bools,  41, 'i');
  AddUserIdent(elx_Globals, 'changegroup                   ', obj_function,  typ_bools,  42, 'bs');
  AddUserIdent(elx_Globals, 'changefilearea                ', obj_procedure, typ_notyp,  43, 's');
  AddUserIdent(elx_Globals, 'changemessageareas            ', obj_procedure, typ_notyp,  44, 's');
  AddUserIdent(elx_Globals, 'showmsgareanewmail            ', obj_procedure, typ_notyp,  45, 'sbbb');
  AddUserIdent(elx_Globals, 'clearcombined                 ', obj_procedure, typ_notyp,  46, 's');
  AddUserIdent(elx_Globals, 'selectcombined                ', obj_procedure, typ_notyp,  47, 's');
  AddUserIdent(elx_Globals, 'togglecombinedarea            ', obj_procedure, typ_notyp,  48, 's');
  AddUserIdent(elx_Globals, 'checkflagaccess               ', obj_function,  typ_bools,  49, 'EEE');
  AddUserIdent(elx_Globals, 'checkfileareaaccess           ', obj_function,  typ_bools,  50, 'Ebbbbi');
  AddUserIdent(elx_Globals, 'checkgroupaccess              ', obj_function,  typ_bools,  51, 'E');
  AddUserIdent(elx_Globals, 'checklanguageaccess           ', obj_function,  typ_bools,  52, 'Eb');
  AddUserIdent(elx_Globals, 'checkmsgareaaccess            ', obj_function,  typ_bools,  53, 'Ebbi');
  AddUserIdent(elx_Globals, 'checkmenuaccess               ', obj_function,  typ_bools,  54, 'e');
  AddUserIdent(elx_Globals, 'checkreadmsgaccess            ', obj_function,  typ_bools,  55, 'sbb');
  AddUserIdent(elx_Globals, 'checkmsgsysopaccess           ', obj_function,  typ_bools,  56, 'E');
  AddUserIdent(elx_Globals, 'givetimeslice                 ', obj_procedure, typ_notyp,  57, '');
  AddUserIdent(elx_Globals, 'setwindowtitle                ', obj_procedure, typ_notyp,  58, 's');
  AddUserIdent(elx_Globals, 'getsystemenvironment          ', obj_function,  typ_strngs, 59, '');
  AddUserIdent(elx_Globals, 'makeattr                      ', obj_function,  typ_ints,   60, 'ii');
  AddUserIdent(elx_Globals, 'getforeattr                   ', obj_function,  typ_ints,   61, 'i');
  AddUserIdent(elx_Globals, 'getbackattr                   ', obj_function,  typ_ints,   62, 'i');
  AddUserIdent(elx_Globals, 'getcolors                     ', obj_procedure, typ_notyp,  63, 'iII');
  AddUserIdent(elx_Globals, 'removeracolors                ', obj_procedure, typ_notyp,  64, 'S');
  AddUserIdent(elx_Globals, 'nocolorlength                 ', obj_function,  typ_ints,   65, 's');
  AddUserIdent(elx_Globals, 'getlastcolor                  ', obj_function,  typ_ints,   66, 's');
  AddUserIdent(elx_Globals, 'fastwrite                     ', obj_procedure, typ_notyp,  67, 'iiis');
  AddUserIdent(elx_Globals, 'boxwindow                     ', obj_procedure, typ_notyp,  68, 'iiiiiis');
  AddUserIdent(elx_Globals, 'colorarea                     ', obj_procedure, typ_notyp,  69, 'iiiii');
  AddUserIdent(elx_Globals, 'fillarea                      ', obj_procedure, typ_notyp,  70, 'iiiici');
  AddUserIdent(elx_Globals, 'localscreen                   ', obj_procedure, typ_notyp,  71, 's');
  AddUserIdent(elx_Globals, 'localscreenln                 ', obj_procedure, typ_notyp,  72, 's');
  AddUserIdent(elx_Globals, 'dobeep                        ', obj_procedure, typ_notyp,  73, 'iiis');
  AddUserIdent(elx_Globals, 'getscreen                     ', obj_procedure, typ_notyp,  74, 'iiCI');
  AddUserIdent(elx_Globals, 'putscreen                     ', obj_procedure, typ_notyp,  75, 'iici');
  AddUserIdent(elx_Globals, 'sendinteractivemsg            ', obj_procedure, typ_notyp,  76, 'is');
  AddUserIdent(elx_Globals, 'checkoktosend                 ', obj_procedure, typ_bools,  77, 'iEb');
  AddUserIdent(elx_Globals, 'doubleuseron                  ', obj_procedure, typ_bools,  78, 's');
  AddUserIdent(elx_Globals, 'getuseron                     ', obj_procedure, typ_ints,   79, 'sb');
  AddUserIdent(elx_Globals, 'emptynodenr                   ', obj_procedure, typ_ints,   80, 'b');
  AddUserIdent(elx_Globals, 'writeuseron                   ', obj_procedure, typ_notyp,  81, 'si');
  AddUserIdent(elx_Globals, 'makedebuglogentry             ', obj_procedure, typ_ints,   82, 'is');
  AddUserIdent(elx_Globals, 'makelogentry                  ', obj_procedure, typ_notyp,  83, 'cs');
  AddUserIdent(elx_Globals, 'loadlimitinfo                 ', obj_procedure, typ_notyp,  84, 'ibb');
  AddUserIdent(elx_Globals, 'lockoutuser                   ', obj_procedure, typ_notyp,  85, '');
  AddUserIdent(elx_Globals, 'settime                       ', obj_procedure, typ_notyp,  86, 'i');
  AddUserIdent(elx_Globals, 'changetime                    ', obj_procedure, typ_notyp,  87, 'ib');
  AddUserIdent(elx_Globals, 'downloadrestrictions          ', obj_procedure, typ_notyp,  88, 'b');
  AddUserIdent(elx_Globals, 'checkcarrierandtime           ', obj_procedure, typ_notyp,  89, '');
  AddUserIdent(elx_Globals, 'timeremain                    ', obj_procedure, typ_notyp,  90, '');
  AddUserIdent(elx_Globals, 'checkeventstatus              ', obj_procedure, typ_notyp,  91, '');
  AddUserIdent(elx_Globals, 'checkraexit                   ', obj_procedure, typ_notyp,  92, '');
  AddUserIdent(elx_Globals, 'checkidle                     ', obj_procedure, typ_notyp,  93, '');
  AddUserIdent(elx_Globals, 'warntimelimit                 ', obj_procedure, typ_notyp,  94, '');
  AddUserIdent(elx_Globals, 'checkforelemon                ', obj_procedure, typ_notyp,  95, '');
  AddUserIdent(elx_Globals, 'timeisupwarning               ', obj_procedure, typ_notyp,  96, '');
  AddUserIdent(elx_Globals, 'resetidletimeout              ', obj_procedure, typ_notyp,  97, '');
  AddUserIdent(elx_Globals, 'setidletimeout                ', obj_procedure, typ_notyp,  98, '');
  AddUserIdent(elx_Globals, 'xxxxxx-01-xxxxxxx             ', obj_procedure, typ_notyp,  99, '');
  AddUserIdent(elx_Globals, 'xxxxxx-02-xxxxxxx             ', obj_procedure, typ_notyp, 100, '');
  AddUserIdent(elx_Globals, 'taggetheader                  ', obj_procedure, typ_notyp, 101, 'iE');
  AddUserIdent(elx_Globals, 'tagputheader                  ', obj_procedure, typ_notyp, 102, 'iE');
  AddUserIdent(elx_Globals, 'tagfixfilename                ', obj_procedure, typ_notyp, 103, 'S');
  AddUserIdent(elx_Globals, 'taggetcurrenttagged           ', obj_function,  typ_ints,  104, '');
  AddUserIdent(elx_Globals, 'taggettotalkbstagged          ', obj_function,  typ_ints,  105, '');
  AddUserIdent(elx_Globals, 'tagistagged                   ', obj_function,  typ_bools, 106, 'si');
  AddUserIdent(elx_Globals, 'tagaddtotagfile               ', obj_function,  typ_bools, 107, 'Eiiis');
  AddUserIdent(elx_Globals, 'tagdeletefromtag              ', obj_procedure, typ_notyp, 108, 's');
  AddUserIdent(elx_Globals, 'tagdeletetagnumber            ', obj_procedure, typ_notyp, 109, 'i');
  AddUserIdent(elx_Globals, 'forceback                     ', obj_function,  typ_strngs,110, 's');
  AddUserIdent(elx_Globals, 'noextension                   ', obj_function,  typ_strngs,111, 's');
  AddUserIdent(elx_Globals, 'justextension                 ', obj_function,  typ_strngs,112, 's');
  AddUserIdent(elx_Globals, 'justpath                      ', obj_function,  typ_strngs,113, 's');
  AddUserIdent(elx_Globals, 'justname                      ', obj_function,  typ_strngs,114, 's');
  AddUserIdent(elx_Globals, 'nobackslash                   ', obj_function,  typ_strngs,115, 's');
  AddUserIdent(elx_Globals, 'rdxname                       ', obj_function,  typ_strngs,116, 's');
  AddUserIdent(elx_Globals, 'fixtime                       ', obj_function,  typ_strngs,117, 's');
  AddUserIdent(elx_Globals, 'slashstr                      ', obj_function,  typ_strngs,118, 's');
  AddUserIdent(elx_Globals, 'asciistr                      ', obj_function,  typ_strngs,119, 's');
  AddUserIdent(elx_Globals, 'under2norm                    ', obj_function,  typ_strngs,120, 's');
  AddUserIdent(elx_Globals, 'space2dot                     ', obj_function,  typ_strngs,121, 's');
  AddUserIdent(elx_Globals, 'trim                          ', obj_function,  typ_strngs,122, 's');
  AddUserIdent(elx_Globals, 'trimleft                      ', obj_function,  typ_strngs,123, 's');
  AddUserIdent(elx_Globals, 'trimright                     ', obj_function,  typ_strngs,124, 's');
  AddUserIdent(elx_Globals, 'firstnameonly                 ', obj_function,  typ_strngs,125, 's');
  AddUserIdent(elx_Globals, 'lastnameonly                  ', obj_function,  typ_strngs,126, 's');
  AddUserIdent(elx_Globals, 'inquotes                      ', obj_function,  typ_strngs,127, 's');
  AddUserIdent(elx_Globals, 'initialstring                 ', obj_function,  typ_strngs,128, 's');
  AddUserIdent(elx_Globals, 'nodoublespace                 ', obj_function,  typ_strngs,129, 's');
  AddUserIdent(elx_Globals, 'fixusername                   ', obj_function,  typ_strngs,130, 's');
  AddUserIdent(elx_Globals, 'strtoreal                     ', obj_function,  typ_reals, 131, 's');
  AddUserIdent(elx_Globals, 'str2flags                     ', obj_function,  typ_ints,  132, 's');
  AddUserIdent(elx_Globals, 'fval                          ', obj_function,  typ_ints,  133, 's');
  AddUserIdent(elx_Globals, 'isftpurl                      ', obj_function,  typ_bools, 134, 's');
  AddUserIdent(elx_Globals, 'lastchar                      ', obj_function,  typ_chars, 135, 's');
  AddUserIdent(elx_Globals, 'firstchar                     ', obj_function,  typ_chars, 136, 's');
  AddUserIdent(elx_Globals, 'xxxxxx-03-xxxxxxx             ', obj_procedure, typ_notyp, 137, '');
  AddUserIdent(elx_Globals, 'xxxxxx-04-xxxxxxx             ', obj_procedure, typ_notyp, 138, '');
  AddUserIdent(elx_Globals, 'xxxxxx-05-xxxxxxx             ', obj_procedure, typ_notyp, 139, '');
  AddUserIdent(elx_Globals, 'hexlong                       ', obj_function,  typ_strngs,140, 'i');
  AddUserIdent(elx_Globals, 'hexstr                        ', obj_function,  typ_strngs,141, 'i');
  AddUserIdent(elx_Globals, 'tohex                         ', obj_function,  typ_strngs,142, 'i');
  AddUserIdent(elx_Globals, 'fstr                          ', obj_function,  typ_strngs,143, 'i');
  AddUserIdent(elx_Globals, 'formatminsec                  ', obj_function,  typ_strngs,144, 'i');
  AddUserIdent(elx_Globals, 'word2time                     ', obj_function,  typ_strngs,145, 'i');
  AddUserIdent(elx_Globals, 'commastr                      ', obj_function,  typ_strngs,146, 'i');
  AddUserIdent(elx_Globals, 'byte2flags                    ', obj_function,  typ_strngs,147, 'i');
  AddUserIdent(elx_Globals, 'byte2flagsoff                 ', obj_function,  typ_strngs,148, 'ii');
  AddUserIdent(elx_Globals, 'fixbaud                       ', obj_function,  typ_ints,  149, 'i');
  AddUserIdent(elx_Globals, 'leadingzero                   ', obj_function,  typ_strngs,150, 'ii');
  AddUserIdent(elx_Globals, 'real2str                      ', obj_function,  typ_strngs,151, 'rii');
  AddUserIdent(elx_Globals, 'dup                           ', obj_function,  typ_strngs,152, 'ci');
  AddUserIdent(elx_Globals, 'xxxxxx-06-xxxxxxx             ', obj_procedure, typ_notyp, 153, '');
  AddUserIdent(elx_Globals, 'xxxxxx-07-xxxxxxx             ', obj_procedure, typ_notyp, 154, '');
  AddUserIdent(elx_Globals, 'xxxxxx-08-xxxxxxx             ', obj_procedure, typ_notyp, 155, '');
  AddUserIdent(elx_Globals, 'xxxxxx-09-xxxxxxx             ', obj_procedure, typ_notyp, 156, '');
  AddUserIdent(elx_Globals, 'xxxxxx-10-xxxxxxx             ', obj_procedure, typ_notyp, 157, '');
  AddUserIdent(elx_Globals, 'xxxxxx-11-xxxxxxx             ', obj_procedure, typ_notyp, 158, '');
  AddUserIdent(elx_Globals, 'xxxxxx-12-xxxxxxx             ', obj_procedure, typ_notyp, 159, '');
  AddUserIdent(elx_Globals, 'makelen                       ', obj_function,  typ_strngs,160, 'siibb');
  AddUserIdent(elx_Globals, 'dotpadding                    ', obj_function,  typ_strngs,161, 'sibc');
  AddUserIdent(elx_Globals, 'getcommaargument              ', obj_function,  typ_strngs,162, 'si');
  AddUserIdent(elx_Globals, 'raducenter                    ', obj_function,  typ_strngs,163, 'si');
  AddUserIdent(elx_Globals, 'rightjust                     ', obj_function,  typ_strngs,164, 'si');
  AddUserIdent(elx_Globals, 'leftjust                      ', obj_function,  typ_strngs,165, 'si');
  AddUserIdent(elx_Globals, 'leftjustchar                  ', obj_function,  typ_strngs,166, 'sic');
  AddUserIdent(elx_Globals, 'centerjust                    ', obj_function,  typ_strngs,167, 'sib');
  AddUserIdent(elx_Globals, 'centerjustchar                ', obj_function,  typ_strngs,168, 'sicb');
  AddUserIdent(elx_Globals, 'rightjustchar                 ', obj_function,  typ_strngs,169, 'sic');
  AddUserIdent(elx_Globals, 'nocolorcopy                   ', obj_function,  typ_strngs,170, 'sii');
  AddUserIdent(elx_Globals, 'addrtostring                  ', obj_procedure, typ_notyp, 171, 'Siiii');
  AddUserIdent(elx_Globals, 'stringtoaddr                  ', obj_procedure, typ_notyp, 172, 'sIIII');
  AddUserIdent(elx_Globals, 'filterstring                  ', obj_procedure, typ_notyp, 173, 'Ss');
  AddUserIdent(elx_Globals, 'removechars                   ', obj_procedure, typ_notyp, 174, 'Ss');
  AddUserIdent(elx_Globals, 'wordcount                     ', obj_function,  typ_ints,  175, 'ssb');
  AddUserIdent(elx_Globals, 'firstword                     ', obj_function,  typ_strngs,176, 'Ssb');
  AddUserIdent(elx_Globals, 'lastword                      ', obj_function,  typ_strngs,177, 'ssb');
  AddUserIdent(elx_Globals, 'removewordnr                  ', obj_procedure, typ_notyp, 178, 'Sisb');
  AddUserIdent(elx_Globals, 'replace                       ', obj_procedure, typ_notyp, 179, 'ssSb');
  AddUserIdent(elx_Globals, 'dowrap                        ', obj_procedure, typ_notyp, 180, 'SSi');
  AddUserIdent(elx_Globals, 'fullinsert                    ', obj_procedure, typ_notyp, 181, 'sSi');
  AddUserIdent(elx_Globals, 'getvalue                      ', obj_function,  typ_strngs,182, 'sSb');
  AddUserIdent(elx_Globals, 'extractword                   ', obj_function,  typ_strngs,183, 'sisbb');
  AddUserIdent(elx_Globals, 'supcase                       ', obj_function,  typ_strngs,184, 's');
  AddUserIdent(elx_Globals, 'slowcase                      ', obj_function,  typ_strngs,185, 's');
  AddUserIdent(elx_Globals, 'firstupper                    ', obj_function,  typ_chars, 186, 's');
  AddUserIdent(elx_Globals, 'strcrc                        ', obj_function,  typ_ints,  187, 's');
  AddUserIdent(elx_Globals, 'replacearray                  ', obj_procedure, typ_notyp, 188, 'SAAb');
  AddUserIdent(elx_Globals, 'md5                           ', obj_function,  typ_strngs,189, 's');
  AddUserIdent(elx_Globals, 'shell                         ', obj_procedure, typ_strngs,190, 's');
  AddUserIdent(elx_Globals, 'xxxxxx-20-xxxxxxx             ', obj_procedure, typ_notyp, 191, '');
  AddUserIdent(elx_Globals, 'xxxxxx-21-xxxxxxx             ', obj_procedure, typ_notyp, 192, '');
  AddUserIdent(elx_Globals, 'xxxxxx-22-xxxxxxx             ', obj_procedure, typ_notyp, 193, '');
  AddUserIdent(elx_Globals, 'xxxxxx-23-xxxxxxx             ', obj_procedure, typ_notyp, 194, '');
  AddUserIdent(elx_Globals, 'xxxxxx-24-xxxxxxx             ', obj_procedure, typ_notyp, 195, '');
  AddUserIdent(elx_Globals, 'xxxxxx-25-xxxxxxx             ', obj_procedure, typ_notyp, 196, '');
  AddUserIdent(elx_Globals, 'xxxxxx-26-xxxxxxx             ', obj_procedure, typ_notyp, 197, '');
  AddUserIdent(elx_Globals, 'xxxxxx-27-xxxxxxx             ', obj_procedure, typ_notyp, 198, '');
  AddUserIdent(elx_Globals, 'xxxxxx-28-xxxxxxx             ', obj_procedure, typ_notyp, 199, '');
  AddUserIdent(elx_Globals, 'xxxxxx-29-xxxxxxx             ', obj_procedure, typ_notyp, 200, '');
  AddUserIdent(elx_Globals, 'gethour                       ', obj_function,  typ_ints,  201, '');
  AddUserIdent(elx_Globals, 'getsecs                       ', obj_function,  typ_ints,  202, '');
  AddUserIdent(elx_Globals, 'getmonth                      ', obj_function,  typ_ints,  203, '');
  AddUserIdent(elx_Globals, 'getyear                       ', obj_function,  typ_ints,  204, '');
  AddUserIdent(elx_Globals, 'getday                        ', obj_function,  typ_ints,  205, '');
  AddUserIdent(elx_Globals, 'getmins                       ', obj_function,  typ_ints,  206, '');
  AddUserIdent(elx_Globals, 'getuserage                    ', obj_function,  typ_ints,  207, '');
  AddUserIdent(elx_Globals, 'getdow                        ', obj_function,  typ_ints,  208, '');
  AddUserIdent(elx_Globals, 'unix2date                     ', obj_function,  typ_ints,  209, 'i');
  AddUserIdent(elx_Globals, 'date2unix                     ', obj_function,  typ_ints,  210, 'i');
  AddUserIdent(elx_Globals, 'norm2unix                     ', obj_function,  typ_ints,  211, 'iiiiii');
  AddUserIdent(elx_Globals, 'nowasunixdate                 ', obj_function,  typ_ints,  212, '');
  AddUserIdent(elx_Globals, 'currenttime                   ', obj_function,  typ_ints,  213, '');
  AddUserIdent(elx_Globals, 'unixtorfcdate                 ', obj_function,  typ_strngs,214, 'is');
  AddUserIdent(elx_Globals, 'getmicrotimer                 ', obj_function,  typ_reals, 215, '');
  AddUserIdent(elx_Globals, 'ceil                          ', obj_function,  typ_reals, 216, 'r');
  AddUserIdent(elx_Globals, 'floor                         ', obj_function,  typ_reals, 217, 'r');
  AddUserIdent(elx_Globals, 'xxxxxx-34-xxxxxxx             ', obj_procedure, typ_notyp, 218, '');
  AddUserIdent(elx_Globals, 'xxxxxx-35-xxxxxxx             ', obj_procedure, typ_notyp, 219, '');
  AddUserIdent(elx_Globals, 'datestr                       ', obj_function,  typ_strngs,220, '');
  AddUserIdent(elx_Globals, 'datestry2k                    ', obj_function,  typ_strngs,221, '');
  AddUserIdent(elx_Globals, 'timestr                       ', obj_function,  typ_strngs,222, 'bb');
  AddUserIdent(elx_Globals, 'date2str                      ', obj_function,  typ_strngs,223, 'l');
  AddUserIdent(elx_Globals, 'getdayofweek                  ', obj_function,  typ_strngs,224, '');
  AddUserIdent(elx_Globals, 'futuredate                    ', obj_function,  typ_strngs,225, 'i');
  AddUserIdent(elx_Globals, 'minsistime                    ', obj_function,  typ_strngs,226, 'i');
  AddUserIdent(elx_Globals, 'isleapyear                    ', obj_function,  typ_bools, 227, 'i');
  AddUserIdent(elx_Globals, 'minstilltime                  ', obj_function,  typ_ints,  228, 's');
  AddUserIdent(elx_Globals, 'minsfromtime                  ', obj_function,  typ_ints,  229, 's');
  AddUserIdent(elx_Globals, 'daysago                       ', obj_function,  typ_ints,  230, 's');
  AddUserIdent(elx_Globals, 'daystogo                      ', obj_function,  typ_ints,  231, 's');
  AddUserIdent(elx_Globals, 'monthnametonum                ', obj_function,  typ_ints,  232, 's');
  AddUserIdent(elx_Globals, 'packtimestr                   ', obj_function,  typ_ints,  233, 'ss');
  AddUserIdent(elx_Globals, 'unix2norm                     ', obj_procedure, typ_notyp, 234, 'iIIIIII');
  AddUserIdent(elx_Globals, 'setstarttime                  ', obj_procedure, typ_notyp, 235, '');
  AddUserIdent(elx_Globals, 'nntpdatetostring              ', obj_procedure, typ_notyp, 236, 'sSS');
  AddUserIdent(elx_Globals, 'historydate                   ', obj_function,  typ_strngs,237, 'i');
  AddUserIdent(elx_Globals, 'gettime                       ', obj_procedure, typ_notyp, 238, 'IIII');
  AddUserIdent(elx_Globals, 'xxxxxx-38-xxxxxxx             ', obj_procedure, typ_notyp, 239, '');
  AddUserIdent(elx_Globals, 'getuserrecordnr               ', obj_procedure, typ_notyp, 240, 'iEEb');
  AddUserIdent(elx_Globals, 'updateuserrecord              ', obj_procedure, typ_notyp, 241, '');
  AddUserIdent(elx_Globals, 'writeuserrecord               ', obj_procedure, typ_notyp, 242, 'iEE');
  AddUserIdent(elx_Globals, 'setnewuserdefaults            ', obj_procedure, typ_notyp, 243, 'EE');
  AddUserIdent(elx_Globals, 'searchuser                    ', obj_function,  typ_ints,  244, 's');
  AddUserIdent(elx_Globals, 'getscreenlength               ', obj_function,  typ_ints,  245, '');
  AddUserIdent(elx_Globals, 'getuserrecord                 ', obj_procedure, typ_notyp, 246, 'EE');
  AddUserIdent(elx_Globals, 'getrecsize                    ', obj_function,  typ_ints,  247, 'E');
  AddUserIdent(elx_Globals, 'xxxxxx-42-xxxxxxx             ', obj_procedure, typ_notyp, 248, '');
  AddUserIdent(elx_Globals, 'xxxxxx-43-xxxxxxx             ', obj_procedure, typ_notyp, 249, '');
  AddUserIdent(elx_Globals, 'filecount                     ', obj_function,  typ_ints,  250, 's');
  AddUserIdent(elx_Globals, 'getpackedfiletime             ', obj_function,  typ_ints,  251, 's');
  AddUserIdent(elx_Globals, 'getfilesize                   ', obj_function,  typ_ints,  252, 'si');
  AddUserIdent(elx_Globals, 'erasefile                     ', obj_function,  typ_bools, 253, 's');
  AddUserIdent(elx_Globals, 'fileexist                     ', obj_function,  typ_bools, 254, 's');
  AddUserIdent(elx_Globals, 'semaexist                     ', obj_function,  typ_bools, 255, 's');
  AddUserIdent(elx_Globals, 'openfile                      ', obj_function,  typ_bools, 256, 'sb');
  AddUserIdent(elx_Globals, 'getfiledate                   ', obj_function,  typ_strngs,257, 's');
  AddUserIdent(elx_Globals, 'getvolumelabel                ', obj_function,  typ_strngs,258, 's');
  AddUserIdent(elx_Globals, 'openrafile                    ', obj_function,  typ_strngs,259, 's');
  AddUserIdent(elx_Globals, 'createtempdir                 ', obj_function,  typ_strngs,260, 'si');
  AddUserIdent(elx_Globals, 'getlogfilename                ', obj_function,  typ_strngs,261, 's');
  AddUserIdent(elx_Globals, 'makedir                       ', obj_procedure, typ_notyp, 262, 's');
  AddUserIdent(elx_Globals, 'xxxxxx-45-xxxxxxx             ', obj_function,  typ_notyp, 263, '');
  AddUserIdent(elx_Globals, 'iswildcard                    ', obj_function,  typ_strngs,264, 'ss');
  AddUserIdent(elx_Globals, 'renamefile                    ', obj_function,  typ_bools, 265, 'ss');
  AddUserIdent(elx_Globals, 'filecopy                      ', obj_function,  typ_bools, 266, 'ss');
  AddUserIdent(elx_Globals, 'searchctlfile                 ', obj_function,  typ_bools, 267, 'ssb');
  AddUserIdent(elx_Globals, 'uptodatefile                  ', obj_function,  typ_bools, 268, 'ss');
  AddUserIdent(elx_Globals, 'groupname                     ', obj_function,  typ_strngs,269, 'ib');
  AddUserIdent(elx_Globals, 'getcurdir                     ', obj_function,  typ_strngs,270, '');
  AddUserIdent(elx_Globals, 'getdiskfree                   ', obj_function,  typ_ints,  271, 'c');
  AddUserIdent(elx_Globals, 'ra250area                     ', obj_function,  typ_ints,  272, 'i');
  AddUserIdent(elx_Globals, 'ra250msgarea                  ', obj_function,  typ_ints,  273, 'i');
  AddUserIdent(elx_Globals, 'searchnextmsgarea             ', obj_function,  typ_ints,  274, 'i');
  AddUserIdent(elx_Globals, 'ra250group                    ', obj_function,  typ_ints,  275, 'ib');
  AddUserIdent(elx_Globals, 'findfirst                     ', obj_function,  typ_ints,  276, 'isi');
  AddUserIdent(elx_Globals, 'findnext                      ', obj_function,  typ_ints,  277, 'i');
  AddUserIdent(elx_Globals, 'findclose                     ', obj_procedure, typ_notyp, 278, 'i');
  AddUserIdent(elx_Globals, 'findgetname                   ', obj_function,  typ_strngs,279, 'i');
  AddUserIdent(elx_Globals, 'writeexitinfo                 ', obj_procedure, typ_notyp, 280, '');
  AddUserIdent(elx_Globals, 'updatestatistics              ', obj_procedure, typ_notyp, 281, '');
  AddUserIdent(elx_Globals, 'buildfastindex                ', obj_procedure, typ_notyp, 282, '');
  AddUserIdent(elx_Globals, 'buildlastread                 ', obj_procedure, typ_notyp, 283, '');
  AddUserIdent(elx_Globals, 'initsystemnames               ', obj_procedure, typ_notyp, 284, '');
  AddUserIdent(elx_Globals, 'dorabusy                      ', obj_procedure, typ_notyp, 285, 'b');
  AddUserIdent(elx_Globals, 'readlanguagera                ', obj_procedure, typ_notyp, 286, 'i');
  AddUserIdent(elx_Globals, 'createdorinfodef              ', obj_procedure, typ_notyp, 287, 'bbi');
  AddUserIdent(elx_Globals, 'createdoorsys                 ', obj_procedure, typ_notyp, 288, 'i');
  AddUserIdent(elx_Globals, 'createdoor32sys               ', obj_procedure, typ_notyp, 289, 'i');
  AddUserIdent(elx_Globals, 'removesema                    ', obj_procedure, typ_notyp, 290, 's');
  AddUserIdent(elx_Globals, 'createsema                    ', obj_procedure, typ_notyp, 291, 's');
  AddUserIdent(elx_Globals, 'killcompletedir               ', obj_procedure, typ_notyp, 292, 'sb');
  AddUserIdent(elx_Globals, 'erasedir                      ', obj_procedure, typ_notyp, 293, 'sb');
  AddUserIdent(elx_Globals, 'setfiletime                   ', obj_procedure, typ_notyp, 294, 's');
  AddUserIdent(elx_Globals, 'createtemplatefile            ', obj_procedure, typ_notyp, 295, 's');
  AddUserIdent(elx_Globals, 'writemsginf                   ', obj_procedure, typ_notyp, 296, 'sssssb');
  AddUserIdent(elx_Globals, 'elecodestr                    ', obj_function,  typ_strngs,297, 's');
  AddUserIdent(elx_Globals, 'getsysopname                  ', obj_function,  typ_strngs,298, '');
  AddUserIdent(elx_Globals, 'getsystemname                 ', obj_function,  typ_strngs,299, '');
  AddUserIdent(elx_Globals, 'xxxxxx-53-xxxxxxx             ', obj_function,  typ_notyp, 300, '');
end; { proc. PartOne }

procedure PartTwo;
begin
  AddUserIdent(elx_Globals, 'getfilesrecord                ', obj_function,  typ_bools, 301, 'Eib');
  AddUserIdent(elx_Globals, 'getelefilesrecord             ', obj_function,  typ_bools, 302, 'Eib');
  AddUserIdent(elx_Globals, 'getmessagerecord              ', obj_function,  typ_bools, 303, 'Eib');
  AddUserIdent(elx_Globals, 'getelemessagerecord           ', obj_function,  typ_bools, 304, 'Eib');
  AddUserIdent(elx_Globals, 'getgrouprecord                ', obj_function,  typ_bools, 305, 'ibbE');
  AddUserIdent(elx_Globals, 'getprotocolrecord             ', obj_procedure, typ_notyp, 306, 'cE');
  AddUserIdent(elx_Globals, 'getexitinfo                   ', obj_procedure, typ_notyp, 307, 'E');
  AddUserIdent(elx_Globals, 'readsysinfobbs                ', obj_procedure, typ_notyp, 308, 'E');
  AddUserIdent(elx_Globals, 'searchnextevent               ', obj_procedure, typ_notyp, 309, 'E');
  AddUserIdent(elx_Globals, 'readexitinfo                  ', obj_procedure, typ_notyp, 310, 'E');
  AddUserIdent(elx_Globals, 'selectallcombined             ', obj_procedure, typ_notyp, 311, 'E');
  AddUserIdent(elx_Globals, 'getaddress                    ', obj_procedure, typ_notyp, 312, 'iE');
  AddUserIdent(elx_Globals, 'setaddress                    ', obj_procedure, typ_notyp, 313, 'iE');
  AddUserIdent(elx_Globals, 'setuserrecord                 ', obj_procedure, typ_notyp, 314, 'EE');
  AddUserIdent(elx_Globals, 'addtouserbase                 ', obj_procedure, typ_notyp, 315, '');
  AddUserIdent(elx_Globals, 'xxxxxx-55-xxxxxxx             ', obj_function,  typ_notyp, 316, '');
  AddUserIdent(elx_Globals, 'xxxxxx-56-xxxxxxx             ', obj_function,  typ_notyp, 317, '');
  AddUserIdent(elx_Globals, 'xxxxxx-57-xxxxxxx             ', obj_function,  typ_notyp, 318, '');
  AddUserIdent(elx_Globals, 'xxxxxx-58-xxxxxxx             ', obj_function,  typ_notyp, 319, '');
  AddUserIdent(elx_Globals, 'gettextattr                   ', obj_function,  typ_ints,  320, '');
  AddUserIdent(elx_Globals, 'getparameter                  ', obj_function,  typ_strngs,321, 'i');
  AddUserIdent(elx_Globals, 'setreturnvalue                ', obj_procedure, typ_notyp, 322, 's');
  AddUserIdent(elx_Globals, 'xxxxxx-59-xxxxxxx             ', obj_function,  typ_notyp, 323, '');
  AddUserIdent(elx_Globals, 'xxxxxx-60-xxxxxxx             ', obj_function,  typ_notyp, 324, '');
  AddUserIdent(elx_Globals, 'xxxxxx-61-xxxxxxx             ', obj_function,  typ_notyp, 325, '');
  AddUserIdent(elx_Globals, 'xxxxxx-62-xxxxxxx             ', obj_function,  typ_notyp, 326, '');
  AddUserIdent(elx_Globals, 'xxxxxx-63-xxxxxxx             ', obj_function,  typ_notyp, 327, '');
  AddUserIdent(elx_Globals, 'xxxxxx-64-xxxxxxx             ', obj_function,  typ_notyp, 328, '');
  AddUserIdent(elx_Globals, 'xxxxxx-65-xxxxxxx             ', obj_function,  typ_notyp, 329, '');
  AddUserIdent(elx_Globals, 'xxxxxx-66-xxxxxxx             ', obj_function,  typ_notyp, 330, '');
  AddUserIdent(elx_Globals, 'fb_openfilebase               ', obj_function,  typ_bools, 331, 'i');
  AddUserIdent(elx_Globals, 'fb_read                       ', obj_function,  typ_bools, 332, 'i');
  AddUserIdent(elx_Globals, 'fb_write                      ', obj_function,  typ_bools, 333, 'i');
  AddUserIdent(elx_Globals, 'fb_totalrecords               ', obj_function,  typ_ints,  334, '');
  AddUserIdent(elx_Globals, 'fb_getfilename                ', obj_function,  typ_strngs,335, '');
  AddUserIdent(elx_Globals, 'fb_getshortname               ', obj_function,  typ_strngs,336, '');
  AddUserIdent(elx_Globals, 'fb_gethdrrecord               ', obj_procedure, typ_notyp, 337, 'E');
  AddUserIdent(elx_Globals, 'fb_sethdrrecord               ', obj_procedure, typ_notyp, 338, 'E');
  AddUserIdent(elx_Globals, 'fb_getidxrecord               ', obj_procedure, typ_notyp, 339, 'E');
  AddUserIdent(elx_Globals, 'fb_setidxrecord               ', obj_procedure, typ_notyp, 340, 'E');
  AddUserIdent(elx_Globals, 'fb_addrecord                  ', obj_procedure, typ_notyp, 341, 'E');
  AddUserIdent(elx_Globals, 'fb_addlfnptr                  ', obj_function,  typ_ints,  342, 'sss');
  AddUserIdent(elx_Globals, 'fb_islfn                      ', obj_function,  typ_bools, 343, 's');
  AddUserIdent(elx_Globals, 'fb_isnewfile                  ', obj_function,  typ_bools, 344, 'is');
  AddUserIdent(elx_Globals, 'fb_matchshowing               ', obj_function,  typ_bools, 345, 'is');
  AddUserIdent(elx_Globals, 'fb_iscomment                  ', obj_function,  typ_bools, 346, '');
  AddUserIdent(elx_Globals, 'fb_isdeleted                  ', obj_function,  typ_bools, 347, '');
  AddUserIdent(elx_Globals, 'fb_isunlisted                 ', obj_function,  typ_bools, 348, '');
  AddUserIdent(elx_Globals, 'fb_isfree                     ', obj_function,  typ_bools, 349, '');
  AddUserIdent(elx_Globals, 'fb_isnotavail                 ', obj_function,  typ_bools, 350, '');
  AddUserIdent(elx_Globals, 'fb_islocked                   ', obj_function,  typ_bools, 351, '');
  AddUserIdent(elx_Globals, 'fb_ismissing                  ', obj_function,  typ_bools, 352, '');
  AddUserIdent(elx_Globals, 'fb_isnotime                   ', obj_function,  typ_bools, 353, '');
  AddUserIdent(elx_Globals, 'fb_doreaddescription          ', obj_procedure, typ_notyp, 354, '');
  AddUserIdent(elx_Globals, 'fb_resetdescriptionptr        ', obj_procedure, typ_notyp, 355, '');
  AddUserIdent(elx_Globals, 'fb_getdescline                ', obj_function,  typ_strngs,356, 'i');
  AddUserIdent(elx_Globals, 'fb_endofdesc                  ', obj_function,  typ_bools, 357, '');
  AddUserIdent(elx_Globals, 'xxxxxx-68-xxxxxxx             ', obj_procedure, typ_notyp, 358, 's');
  AddUserIdent(elx_Globals, 'fb_unlistedfile               ', obj_function,  typ_bools, 359, '');
  AddUserIdent(elx_Globals, 'fb_addtotagarray              ', obj_procedure, typ_notyp, 360, 'ii');
  AddUserIdent(elx_Globals, 'fb_geterror                   ', obj_function,  typ_ints,  361, '');
  AddUserIdent(elx_Globals, 'fb_closefilebase              ', obj_procedure, typ_notyp, 362, '');
  AddUserIdent(elx_Globals, 'fb_tagfiles                   ', obj_procedure, typ_notyp, 363, '');
  AddUserIdent(elx_Globals, 'fb_viewfiles                  ', obj_procedure, typ_notyp, 364, '');
  AddUserIdent(elx_Globals, 'fb_edittaglist                ', obj_procedure, typ_notyp, 365, '');
  AddUserIdent(elx_Globals, 'fb_newsincelastlogon          ', obj_function,  typ_bools, 366, '');
  AddUserIdent(elx_Globals, 'fb_dumpbinaryfile             ', obj_procedure, typ_notyp, 367, 's');
  AddUserIdent(elx_Globals, 'xxxxxx-68-xxxxxxx             ', obj_function,  typ_notyp, 368, '');
  AddUserIdent(elx_Globals, 'xxxxxx-69-xxxxxxx             ', obj_function,  typ_notyp, 369, '');
  AddUserIdent(elx_Globals, 'web_getuin                    ', obj_function,  typ_ints,  370, '');
  AddUserIdent(elx_Globals, 'web_getuir                    ', obj_function,  typ_ints,  371, '');
  AddUserIdent(elx_Globals, 'web_getuip                    ', obj_function,  typ_ints,  372, '');
  AddUserIdent(elx_Globals, 'web_showhtmlfile              ', obj_function,  typ_bools, 373, 's');
  AddUserIdent(elx_Globals, 'web_getdatetime               ', obj_function,  typ_strngs,374, '');
  AddUserIdent(elx_Globals, 'web_getcookie                 ', obj_function,  typ_strngs,375, 's');
  AddUserIdent(elx_Globals, 'web_setlogindata              ', obj_procedure, typ_notyp, 376, 'iiib');
  AddUserIdent(elx_Globals, 'web_getformdata               ', obj_function,  typ_strngs,377, 's');
  AddUserIdent(elx_Globals, 'web_foundfield                ', obj_function,  typ_ints,  378, 's');
  AddUserIdent(elx_Globals, 'web_isloggedon                ', obj_function,  typ_bools, 379, '');
  AddUserIdent(elx_Globals, 'web_runerrorscript            ', obj_procedure, typ_notyp, 380, 'is');
  AddUserIdent(elx_Globals, 'web_runscript                 ', obj_function,  typ_bools, 381, 'ss');
  AddUserIdent(elx_Globals, 'web_convertlink               ', obj_function,  typ_strngs,382, 's');
  AddUserIdent(elx_Globals, 'web_openoutput                ', obj_procedure, typ_notyp, 383, 's');
  AddUserIdent(elx_Globals, 'web_esctononesc               ', obj_function,  typ_strngs,384, 's');
  AddUserIdent(elx_Globals, 'web_sendheader                ', obj_procedure, typ_notyp, 385, 'ss');
  AddUserIdent(elx_Globals, 'web_getbigformdata            ', obj_function,  typ_strngs,386, 's');
  AddUserIdent(elx_Globals, 'web_sendcookie                ', obj_procedure, typ_notyp, 387, 's');
  AddUserIdent(elx_Globals, 'web_matchcredentials          ', obj_function,  typ_bools, 388, 'iiiI');
  AddUserIdent(elx_Globals, 'web_cookieexist               ', obj_function,  typ_bools, 389, 's');
  AddUserIdent(elx_Globals, 'xxxxxx-79-xxxxxxx             ', obj_function,  typ_notyp, 390, '');
  AddUserIdent(elx_Globals, 'xxxxxx-80-xxxxxxx             ', obj_function,  typ_notyp, 391, '');
  AddUserIdent(elx_Globals, 'xxxxxx-81-xxxxxxx             ', obj_function,  typ_notyp, 392, '');
  AddUserIdent(elx_Globals, 'xxxxxx-82-xxxxxxx             ', obj_function,  typ_notyp, 393, '');
  AddUserIdent(elx_Globals, 'xxxxxx-83-xxxxxxx             ', obj_function,  typ_notyp, 394, '');
  AddUserIdent(elx_Globals, 'xxxxxx-84-xxxxxxx             ', obj_function,  typ_notyp, 395, '');
  AddUserIdent(elx_Globals, 'xxxxxx-85-xxxxxxx             ', obj_function,  typ_notyp, 396, '');
  AddUserIdent(elx_Globals, 'xxxxxx-86-xxxxxxx             ', obj_function,  typ_notyp, 397, '');
  AddUserIdent(elx_Globals, 'xxxxxx-87-xxxxxxx             ', obj_function,  typ_notyp, 398, '');
  AddUserIdent(elx_Globals, 'xxxxxx-88-xxxxxxx             ', obj_function,  typ_notyp, 399, '');
  AddUserIdent(elx_Globals, 'xxxxxx-89-xxxxxxx             ', obj_function,  typ_notyp, 400, '');
  AddUserIdent(elx_Globals, 'getpath_protocolra            ', obj_function,  typ_strngs,401, '');
  AddUserIdent(elx_Globals, 'getpath_languagera            ', obj_function,  typ_strngs,402, '');
  AddUserIdent(elx_Globals, 'getpath_limitsra              ', obj_function,  typ_strngs,403, '');
  AddUserIdent(elx_Globals, 'getpath_eventsra              ', obj_function,  typ_strngs,404, '');
  AddUserIdent(elx_Globals, 'getpath_fgroupsra             ', obj_function,  typ_strngs,405, '');
  AddUserIdent(elx_Globals, 'getpath_mgroupsra             ', obj_function,  typ_strngs,406, '');
  AddUserIdent(elx_Globals, 'getpath_filesra               ', obj_function,  typ_strngs,407, '');
  AddUserIdent(elx_Globals, 'getpath_akasbbs               ', obj_function,  typ_strngs,408, '');
  AddUserIdent(elx_Globals, 'getpath_modemra               ', obj_function,  typ_strngs,409, '');
  AddUserIdent(elx_Globals, 'getpath_telnetele             ', obj_function,  typ_strngs,410, '');
  AddUserIdent(elx_Globals, 'getpath_pagestatra            ', obj_function,  typ_strngs,411, '');
  AddUserIdent(elx_Globals, 'getpath_messagesra            ', obj_function,  typ_strngs,412, '');
  AddUserIdent(elx_Globals, 'getpath_taglistra             ', obj_function,  typ_strngs,413, '');
  AddUserIdent(elx_Globals, 'getpath_lastcallbbs           ', obj_function,  typ_strngs,414, '');
  AddUserIdent(elx_Globals, 'getpath_sysinfobbs            ', obj_function,  typ_strngs,415, '');
  AddUserIdent(elx_Globals, 'getpath_timelogbbs            ', obj_function,  typ_strngs,416, '');
  AddUserIdent(elx_Globals, 'getpath_logfile               ', obj_function,  typ_strngs,417, '');
  AddUserIdent(elx_Globals, 'getpath_messagesele           ', obj_function,  typ_strngs,418, '');
  AddUserIdent(elx_Globals, 'getpath_filesele              ', obj_function,  typ_strngs,419, '');
  AddUserIdent(elx_Globals, 'getpath_nwserverele           ', obj_function,  typ_strngs,420, '');
  AddUserIdent(elx_Globals, 'getpath_htmlpath              ', obj_function,  typ_strngs,421, '');
  AddUserIdent(elx_Globals, 'getpath_scrpath               ', obj_function,  typ_strngs,422, '');
  AddUserIdent(elx_Globals, 'getpath_syspath               ', obj_function,  typ_strngs,423, '');
  AddUserIdent(elx_Globals, 'getpath_msgbasepath           ', obj_function,  typ_strngs,424, '');
  AddUserIdent(elx_Globals, 'getpath_filebasepath          ', obj_function,  typ_strngs,425, '');
  AddUserIdent(elx_Globals, 'xxxxxx-94-xxxxxxx             ', obj_function,  typ_notyp, 426, '');
  AddUserIdent(elx_Globals, 'xxxxxx-95-xxxxxxx             ', obj_function,  typ_notyp, 427, '');
  AddUserIdent(elx_Globals, 'xxxxxx-96-xxxxxxx             ', obj_function,  typ_notyp, 428, '');
  AddUserIdent(elx_Globals, 'xxxxxx-97-xxxxxxx             ', obj_function,  typ_notyp, 429, '');
  AddUserIdent(elx_Globals, 'xxxxxx-99-xxxxxxx             ', obj_function,  typ_notyp, 430, '');
  AddUserIdent(elx_Globals, 'xxxxxx-99-xxxxxxx             ', obj_function,  typ_notyp, 431, '');
  AddUserIdent(elx_Globals, 'xxxxxx-100-xxxxxx             ', obj_function,  typ_notyp, 432, '');
  AddUserIdent(elx_Globals, 'xxxxxx-101-xxxxxx             ', obj_function,  typ_notyp, 433, '');
  AddUserIdent(elx_Globals, 'xxxxxx-102-xxxxxx             ', obj_function,  typ_notyp, 434, '');
  AddUserIdent(elx_Globals, 'xxxxxx-103-xxxxxx             ', obj_function,  typ_notyp, 435, '');
  AddUserIdent(elx_Globals, 'xxxxxx-104-xxxxxx             ', obj_function,  typ_notyp, 436, '');
  AddUserIdent(elx_Globals, 'xxxxxx-105-xxxxxx             ', obj_function,  typ_notyp, 437, '');
  AddUserIdent(elx_Globals, 'xxxxxx-106-xxxxxx             ', obj_function,  typ_notyp, 438, '');
  AddUserIdent(elx_Globals, 'xxxxxx-107-xxxxxx             ', obj_function,  typ_notyp, 439, '');
  AddUserIdent(elx_Globals, 'mb_getareaaddress             ', obj_function,  typ_strngs,440, 'i');
  AddUserIdent(elx_Globals, 'mb_inmarkedlist               ', obj_function,  typ_bools, 441, 'ii');
  AddUserIdent(elx_Globals, 'mb_addtomarklist              ', obj_procedure, typ_notyp, 442, 'ii');
  AddUserIdent(elx_Globals, 'mb_setlastread                ', obj_procedure, typ_notyp, 443, 'ii');
  AddUserIdent(elx_Globals, 'mb_openmsgbase                ', obj_function,  typ_bools, 444, 'E');
  AddUserIdent(elx_Globals, 'mb_read                       ', obj_procedure, typ_notyp, 445, 'i');
  AddUserIdent(elx_Globals, 'mb_write                      ', obj_procedure, typ_notyp, 446, 'i');
  AddUserIdent(elx_Globals, 'mb_messagefound               ', obj_function,  typ_bools, 447, '');
  AddUserIdent(elx_Globals, 'mb_getprevious                ', obj_procedure, typ_notyp, 448, '');
  AddUserIdent(elx_Globals, 'mb_getnext                    ', obj_procedure, typ_notyp, 449, '');
  AddUserIdent(elx_Globals, 'mb_getrecord                  ', obj_procedure, typ_notyp, 450, 'E');
  AddUserIdent(elx_Globals, 'mb_setrecord                  ', obj_procedure, typ_notyp, 451, 'E');
  AddUserIdent(elx_Globals, 'mb_addmessage                 ', obj_procedure, typ_notyp, 452, 'E');
  AddUserIdent(elx_Globals, 'mb_getactivemsgnum            ', obj_function,  typ_ints,  453, '');
  AddUserIdent(elx_Globals, 'mb_gethighmsgnum              ', obj_function,  typ_ints,  454, '');
  AddUserIdent(elx_Globals, 'mb_getnetmailboard            ', obj_function,  typ_ints,  455, '');
  AddUserIdent(elx_Globals, 'mb_getmsgareastats            ', obj_procedure, typ_notyp, 456, 'EIII');
  AddUserIdent(elx_Globals, 'mb_silentdeletemsg            ', obj_procedure, typ_notyp, 457, 'iib');
  AddUserIdent(elx_Globals, 'mb_menupost                   ', obj_procedure, typ_notyp, 458, 's');
  AddUserIdent(elx_Globals, 'mb_postfile                   ', obj_procedure, typ_notyp, 459, 'issss');
  AddUserIdent(elx_Globals, 'mb_doreadmessagetext          ', obj_procedure, typ_notyp, 460, '');
  AddUserIdent(elx_Globals, 'mb_resetmessageptr            ', obj_procedure, typ_notyp, 461, '');
  AddUserIdent(elx_Globals, 'mb_endofmessage               ', obj_function,  typ_bools, 462, '');
  AddUserIdent(elx_Globals, 'mb_getmessageline             ', obj_function,  typ_strngs,463, 'i');
  AddUserIdent(elx_Globals, 'mb_htmlstring                 ', obj_function,  typ_strngs,464, 's');
  AddUserIdent(elx_Globals, 'mb_addmessageline             ', obj_procedure, typ_notyp, 465, 's');
  AddUserIdent(elx_Globals, 'mb_settowho                   ', obj_procedure, typ_notyp, 466, 's');
  AddUserIdent(elx_Globals, 'mb_setfromwho                 ', obj_procedure, typ_notyp, 467, 's');
  AddUserIdent(elx_Globals, 'mb_setsubject                 ', obj_procedure, typ_notyp, 468, 's');
  AddUserIdent(elx_Globals, 'mb_setreturnreceipt           ', obj_procedure, typ_notyp, 469, 'b');
  AddUserIdent(elx_Globals, 'mb_setprivate                 ', obj_procedure, typ_notyp, 470, 'b');
  AddUserIdent(elx_Globals, 'mb_setreplyreceipt            ', obj_procedure, typ_notyp, 471, 'b');
  AddUserIdent(elx_Globals, 'mb_setkillsent                ', obj_procedure, typ_notyp, 472, 'b');
  AddUserIdent(elx_Globals, 'mb_setcrashmail               ', obj_procedure, typ_notyp, 473, 'b');
  AddUserIdent(elx_Globals, 'mb_setattachment              ', obj_procedure, typ_notyp, 474, 'b');
  AddUserIdent(elx_Globals, 'mb_setmarkassent              ', obj_procedure, typ_notyp, 475, 'b');
  AddUserIdent(elx_Globals, 'mb_getmsgnumber               ', obj_function,  typ_ints,  476, '');
  AddUserIdent(elx_Globals, 'mb_setdestaddress             ', obj_procedure, typ_notyp, 477, 's');
  AddUserIdent(elx_Globals, 'mb_setorigaddress             ', obj_procedure, typ_notyp, 478, 's');
  AddUserIdent(elx_Globals, 'mb_handlemessageread          ', obj_procedure, typ_notyp, 479, 'bE');
  AddUserIdent(elx_Globals, 'mb_setreceived                ', obj_procedure, typ_notyp, 480, 'b');
  AddUserIdent(elx_Globals, 'mb_setdatestr                 ', obj_procedure, typ_notyp, 481, 's');
  AddUserIdent(elx_Globals, 'mb_settimestr                 ', obj_procedure, typ_notyp, 482, 's');
  AddUserIdent(elx_Globals, 'mb_setreplynr                 ', obj_procedure, typ_notyp, 483, 'iii');
  AddUserIdent(elx_Globals, 'mb_gettowho                   ', obj_function,  typ_strngs,484, '');
  AddUserIdent(elx_Globals, 'mb_getfromwho                 ', obj_function,  typ_strngs,485, '');
  AddUserIdent(elx_Globals, 'mb_getsubject                 ', obj_function,  typ_strngs,486, '');
  AddUserIdent(elx_Globals, 'mb_getreturnreceipt           ', obj_function,  typ_bools, 487, '');
  AddUserIdent(elx_Globals, 'mb_getprivate                 ', obj_function,  typ_bools, 488, '');
  AddUserIdent(elx_Globals, 'mb_getreplyreceipt            ', obj_function,  typ_bools, 489, '');
  AddUserIdent(elx_Globals, 'mb_getkillsent                ', obj_function,  typ_bools, 490, '');
  AddUserIdent(elx_Globals, 'mb_getcrashmail               ', obj_function,  typ_bools, 491, '');
  AddUserIdent(elx_Globals, 'mb_getattachment              ', obj_function,  typ_bools, 492, '');
  AddUserIdent(elx_Globals, 'mb_getmarkassent              ', obj_function,  typ_bools, 493, '');
  AddUserIdent(elx_Globals, 'mb_getmsglines                ', obj_function,  typ_ints,  494, '');
  AddUserIdent(elx_Globals, 'mb_getdestaddress             ', obj_function,  typ_strngs,495, '');
  AddUserIdent(elx_Globals, 'mb_getorigaddress             ', obj_function,  typ_strngs,496, '');
  AddUserIdent(elx_Globals, 'mb_getmarkasreceived          ', obj_function,  typ_bools, 497, '');
  AddUserIdent(elx_Globals, 'mb_getdatestr                 ', obj_function,  typ_strngs,498, '');
  AddUserIdent(elx_Globals, 'mb_gettimestr                 ', obj_function,  typ_strngs,499, '');
  AddUserIdent(elx_Globals, 'mb_hasnewmail                 ', obj_function,  typ_bools, 500, 'E');
  AddUserIdent(elx_Globals, 'mb_getlastread                ', obj_function,  typ_ints,  501, 'E');
  AddUserIdent(elx_Globals, 'mb_closemessagebase           ', obj_procedure, typ_notyp, 502, '');
  AddUserIdent(elx_Globals, 'checkmsgdeleteaccess          ', obj_function,  typ_bools, 503, 'Essb');
  AddUserIdent(elx_Globals, 'mb_postmessage                ', obj_procedure, typ_notyp, 504, 'issssssbbbbbbI');
  AddUserIdent(elx_Globals, 'mb_strbuf_init                ', obj_procedure, typ_notyp, 505, '');
  AddUserIdent(elx_Globals, 'mb_strbuf_get                 ', obj_function,  typ_strngs,506, 'i');
  AddUserIdent(elx_Globals, 'mb_strbuf_put                 ', obj_procedure, typ_notyp, 507, 'is');
  AddUserIdent(elx_Globals, 'mb_strbuf_delete              ', obj_procedure, typ_notyp, 508, 'i');
  AddUserIdent(elx_Globals, 'mb_strbuf_add                 ', obj_procedure, typ_notyp, 509, 's');
  AddUserIdent(elx_Globals, 'mb_strbuf_done                ', obj_procedure, typ_notyp, 510, '');
  AddUserIdent(elx_Globals, 'mb_strbuf_count               ', obj_function,  typ_ints,  511, '');
  AddUserIdent(elx_Globals, 'mb_strbuf_clear               ', obj_procedure, typ_notyp, 512, '');
  AddUserIdent(elx_Globals, 'mb_form_to_strbuf             ', obj_procedure, typ_notyp, 513, 'sb');
  AddUserIdent(elx_Globals, 'getnewsarticleheader          ', obj_procedure, typ_notyp, 514, 'E');
  AddUserIdent(elx_Globals, 'mb_getreplyfirst              ', obj_function,  typ_ints,  515, '');
  AddUserIdent(elx_Globals, 'mb_getreplynext               ', obj_function,  typ_ints,  516, '');
  AddUserIdent(elx_Globals, 'mb_getreplyto                 ', obj_function,  typ_ints,  517, '');
  AddUserIdent(elx_Globals, 'formatdatestring              ', obj_function,  typ_strngs,518, 'sii');
  AddUserIdent(elx_Globals, 'mb_updatemsgtext              ', obj_function,  typ_ints,  519, '');
  AddUserIdent(elx_Globals, 'usr_getextensions             ', obj_procedure, typ_notyp, 520, 'E');
  AddUserIdent(elx_Globals, 'usr_setextensions             ', obj_procedure, typ_notyp, 521, 'E');
  AddUserIdent(elx_Globals, 'mb_allowedit                  ', obj_function,  typ_bools, 522, 'Es');
  AddUserIdent(elx_Globals, 'mb_setreplyfirst              ', obj_procedure, typ_notyp, 523, 'i');
  AddUserIdent(elx_Globals, 'mb_setreplynext               ', obj_procedure, typ_notyp, 524, 'i');
  AddUserIdent(elx_Globals, 'mb_setreplyto                 ', obj_procedure, typ_notyp, 525, 'i');
  AddUserIdent(elx_Globals, 'usr_extsetdefaults            ', obj_procedure, typ_notyp, 526, 'E');
  AddUserIdent(elx_Globals, 'xxxxxx-117-xxxxxx             ', obj_function,  typ_notyp, 527, '');
  AddUserIdent(elx_Globals, 'xxxxxx-118-xxxxxx             ', obj_function,  typ_notyp, 528, '');
  AddUserIdent(elx_Globals, 'xxxxxx-119-xxxxxx             ', obj_function,  typ_notyp, 529, '');
  AddUserIdent(elx_Globals, 'xxxxxx-120-xxxxxx             ', obj_function,  typ_notyp, 530, '');
  AddUserIdent(elx_Globals, 'sock_init                     ', obj_procedure, typ_notyp, 531, 'i');
  AddUserIdent(elx_Globals, 'sock_done                     ', obj_procedure, typ_notyp, 532, 'i');
  AddUserIdent(elx_Globals, 'sock_connectto                ', obj_function,  typ_bools, 533, 'isiS');
  AddUserIdent(elx_Globals, 'sock_gethandle                ', obj_function,  typ_ints,  534, 'i');
  AddUserIdent(elx_Globals, 'sock_dataavailable            ', obj_function,  typ_bools, 535, 'i');
  AddUserIdent(elx_Globals, 'sock_connectionalive          ', obj_function,  typ_bools, 536, 'i');
  AddUserIdent(elx_Globals, 'sock_sendstrln                ', obj_procedure, typ_notyp, 537, 'is');
  AddUserIdent(elx_Globals, 'sock_sendstr                  ', obj_procedure, typ_notyp, 538, 'is');
  AddUserIdent(elx_Globals, 'sock_sendbuf                  ', obj_procedure, typ_notyp, 539, 'iEiI');
  AddUserIdent(elx_Globals, 'sock_recvbuf                  ', obj_procedure, typ_notyp, 540, 'iEiI');
  AddUserIdent(elx_Globals, 'sock_disconnect               ', obj_procedure, typ_notyp, 541, 'i');
  AddUserIdent(elx_Globals, 'sock_waitfordata              ', obj_procedure, typ_notyp, 542, 'i');
  AddUserIdent(elx_Globals, 'sock_flushdata                ', obj_procedure, typ_notyp, 543, 'i');
  AddUserIdent(elx_Globals, 'sock_dosleep                  ', obj_procedure, typ_notyp, 544, 'ii');
  AddUserIdent(elx_Globals, 'sock_recvstrln                ', obj_function,  typ_bools, 545, 'iSb');
  AddUserIdent(elx_Globals, 'sock_recvstring               ', obj_function,  typ_bools, 546, 'iSb');
  AddUserIdent(elx_Globals, 'sock_resolveaddr              ', obj_function,  typ_bools, 547, 'isS');
  AddUserIdent(elx_Globals, 'sock_connectedip              ', obj_function,  typ_bools, 548, 'i');
  AddUserIdent(elx_Globals, 'sock_setkeepalive             ', obj_procedure, typ_notyp, 549, 'ib');
  AddUserIdent(elx_Globals, 'sock_getkeepalive             ', obj_function,  typ_bools, 550, 'ib');
  AddUserIdent(elx_Globals, 'sock_setreuseaddr             ', obj_procedure, typ_notyp, 551, 'ib');
  AddUserIdent(elx_Globals, 'sock_getreuseaddr             ', obj_function,  typ_bools, 552, 'ib');
  AddUserIdent(elx_Globals, 'sock_setserverhandle          ', obj_procedure, typ_notyp, 553, 'ii');
  AddUserIdent(elx_Globals, 'sock_getserverhandle          ', obj_function,  typ_ints,  554, 'ii');
  AddUserIdent(elx_Globals, 'sock_setupserver              ', obj_function,  typ_bools, 555, 'iiSi');
  AddUserIdent(elx_Globals, 'sock_accept                   ', obj_function,  typ_ints,  556, 'iEi');
  AddUserIdent(elx_Globals, 'sock_geterror                 ', obj_function,  typ_ints,  557, '');
  AddUserIdent(elx_Globals, 'sock_setsockethandle          ', obj_procedure, typ_notyp, 558, 'ii');
  AddUserIdent(elx_Globals, 'sock_setblocking              ', obj_procedure, typ_notyp, 559, 'ib');
  AddUserIdent(elx_Globals, 'sock_getblocking              ', obj_function,  typ_bools, 560, 'ib');
  AddUserIdent(elx_Globals, 'debug_stringfreecount         ', obj_function,  typ_ints,  561, '');
  AddUserIdent(elx_Globals, 'xxxxxx-122-xxxxxx             ', obj_function,  typ_notyp, 562, '');
  AddUserIdent(elx_Globals, 'xxxxxx-123-xxxxxx             ', obj_function,  typ_notyp, 563, '');
  AddUserIdent(elx_Globals, 'xxxxxx-124-xxxxxx             ', obj_function,  typ_notyp, 564, '');
  AddUserIdent(elx_Globals, 'xxxxxx-125-xxxxxx             ', obj_function,  typ_notyp, 565, '');
  AddUserIdent(elx_Globals, 'xxxxxx-126-xxxxxx             ', obj_function,  typ_notyp, 566, '');
  AddUserIdent(elx_Globals, 'xxxxxx-127-xxxxxx             ', obj_function,  typ_notyp, 567, '');
  AddUserIdent(elx_Globals, 'xxxxxx-128-xxxxxx             ', obj_function,  typ_notyp, 568, '');
  AddUserIdent(elx_Globals, 'xxxxxx-129-xxxxxx             ', obj_function,  typ_notyp, 569, '');
  AddUserIdent(elx_Globals, 'xxxxxx-130-xxxxxx             ', obj_function,  typ_notyp, 570, '');
  AddUserIdent(elx_Globals, 'xxxxxx-131-xxxxxx             ', obj_function,  typ_notyp, 571, '');
  AddUserIdent(elx_Globals, 'xxxxxx-132-xxxxxx             ', obj_function,  typ_notyp, 572, '');
  AddUserIdent(elx_Globals, 'xxxxxx-133-xxxxxx             ', obj_function,  typ_notyp, 573, '');
  AddUserIdent(elx_Globals, 'xxxxxx-134-xxxxxx             ', obj_function,  typ_notyp, 574, '');
  AddUserIdent(elx_Globals, 'xxxxxx-135-xxxxxx             ', obj_function,  typ_notyp, 575, '');
  AddUserIdent(elx_Globals, 'xxxxxx-136-xxxxxx             ', obj_function,  typ_notyp, 576, '');
  AddUserIdent(elx_Globals, 'xxxxxx-137-xxxxxx             ', obj_function,  typ_notyp, 577, '');
  AddUserIdent(elx_Globals, 'xxxxxx-138-xxxxxx             ', obj_function,  typ_notyp, 578, '');
  AddUserIdent(elx_Globals, 'xxxxxx-139-xxxxxx             ', obj_function,  typ_notyp, 579, '');
  AddUserIdent(elx_Globals, 'xxxxxx-140-xxxxxx             ', obj_function,  typ_notyp, 580, '');
  AddUserIdent(elx_Globals, 'xxxxxx-141-xxxxxx             ', obj_function,  typ_notyp, 581, '');
  AddUserIdent(elx_Globals, 'xxxxxx-142-xxxxxx             ', obj_function,  typ_notyp, 582, '');
  AddUserIdent(elx_Globals, 'xxxxxx-143-xxxxxx             ', obj_function,  typ_notyp, 583, '');
  AddUserIdent(elx_Globals, 'xxxxxx-144-xxxxxx             ', obj_function,  typ_notyp, 584, '');
  AddUserIdent(elx_Globals, 'xxxxxx-145-xxxxxx             ', obj_function,  typ_notyp, 585, '');
  AddUserIdent(elx_Globals, 'xxxxxx-146-xxxxxx             ', obj_function,  typ_notyp, 586, '');
  AddUserIdent(elx_Globals, 'xxxxxx-147-xxxxxx             ', obj_function,  typ_notyp, 587, '');
  AddUserIdent(elx_Globals, 'xxxxxx-148-xxxxxx             ', obj_function,  typ_notyp, 588, '');
  AddUserIdent(elx_Globals, 'xxxxxx-149-xxxxxx             ', obj_function,  typ_notyp, 589, '');
  AddUserIdent(elx_Globals, 'xxxxxx-150-xxxxxx             ', obj_function,  typ_notyp, 590, '');
  AddUserIdent(elx_Globals, 'xxxxxx-151-xxxxxx             ', obj_function,  typ_notyp, 591, '');
  AddUserIdent(elx_Globals, 'xxxxxx-152-xxxxxx             ', obj_function,  typ_notyp, 592, '');
  AddUserIdent(elx_Globals, 'xxxxxx-153-xxxxxx             ', obj_function,  typ_notyp, 593, '');
  AddUserIdent(elx_Globals, 'xxxxxx-154-xxxxxx             ', obj_function,  typ_notyp, 594, '');
  AddUserIdent(elx_Globals, 'xxxxxx-155-xxxxxx             ', obj_function,  typ_notyp, 595, '');
  AddUserIdent(elx_Globals, 'xxxxxx-156-xxxxxx             ', obj_function,  typ_notyp, 596, '');
  AddUserIdent(elx_Globals, 'xxxxxx-157-xxxxxx             ', obj_function,  typ_notyp, 597, '');
  AddUserIdent(elx_Globals, 'xxxxxx-158-xxxxxx             ', obj_function,  typ_notyp, 598, '');
  AddUserIdent(elx_Globals, 'xxxxxx-159-xxxxxx             ', obj_function,  typ_notyp, 599, '');
  AddUserIdent(elx_Globals, 'xxxxxx-160-xxxxxx             ', obj_function,  typ_notyp, 600, '');
end; { proc. PartTwo }

procedure PartThree;
begin
  {$IFDEF elx_mysql}
  AddUserIdent(elx_Globals, 'mysql_num_rows                ', obj_function,  typ_ints,  601, 'i');
  AddUserIdent(elx_Globals, 'mysql_num_fields              ', obj_function,  typ_ints,  602, 'i');
  AddUserIdent(elx_Globals, 'mysql_eof                     ', obj_function,  typ_bools, 603, 'i');
  AddUserIdent(elx_Globals, 'mysql_field_tell              ', obj_function,  typ_ints,  604, 'i');
  AddUserIdent(elx_Globals, 'mysql_affected_rows           ', obj_function,  typ_ints,  605, 'i');
  AddUserIdent(elx_Globals, 'mysql_insert_id               ', obj_function,  typ_ints,  606, 'i');
  AddUserIdent(elx_Globals, 'mysql_errorno                 ', obj_function,  typ_ints,  607, 'i');
  AddUserIdent(elx_Globals, 'mysql_info                    ', obj_function,  typ_strngs,608, 'i');
  AddUserIdent(elx_Globals, 'mysql_reload                  ', obj_function,  typ_ints,  609, 'i');
  AddUserIdent(elx_Globals, 'mysql_thread_id               ', obj_function,  typ_ints,  610, 'i');
  AddUserIdent(elx_Globals, 'mysql_error                   ', obj_function,  typ_strngs,611, 'i');
  AddUserIdent(elx_Globals, 'mysql_close                   ', obj_function,  typ_ints,  612, 'i');
  AddUserIdent(elx_Globals, 'mysql_select_db               ', obj_function,  typ_ints,  613, 'is');
  AddUserIdent(elx_Globals, 'mysql_query                   ', obj_function,  typ_ints,  614, 'is');
  AddUserIdent(elx_Globals, 'mysql_real_query              ', obj_function,  typ_ints,  615, 'isi');
  AddUserIdent(elx_Globals, 'mysql_create_db               ', obj_function,  typ_ints,  616, 'is');
  AddUserIdent(elx_Globals, 'mysql_drop_db                 ', obj_function,  typ_ints,  617, 'is');
  AddUserIdent(elx_Globals, 'mysql_shutdown                ', obj_function,  typ_ints,  618, 'i');
  AddUserIdent(elx_Globals, 'mysql_dump_debug_info         ', obj_function,  typ_ints,  619, 'i');
  AddUserIdent(elx_Globals, 'mysql_refresh                 ', obj_function,  typ_ints,  620, 'ii');
  AddUserIdent(elx_Globals, 'mysql_kill                    ', obj_function,  typ_ints,  621, 'ii');
  AddUserIdent(elx_Globals, 'mysql_stat                    ', obj_function,  typ_strngs,622, 'i');
  AddUserIdent(elx_Globals, 'mysql_get_server_info         ', obj_function,  typ_strngs,623, 'i');
  AddUserIdent(elx_Globals, 'mysql_get_client_info         ', obj_function,  typ_strngs,624, '');
  AddUserIdent(elx_Globals, 'mysql_get_host_info           ', obj_function,  typ_strngs,625, 'i');
  AddUserIdent(elx_Globals, 'mysql_get_proto_info          ', obj_function,  typ_ints,  626, 'i');
  AddUserIdent(elx_Globals, 'mysql_free_result             ', obj_procedure, typ_notyp, 627, 'i');
  AddUserIdent(elx_Globals, 'mysql_data_seek               ', obj_procedure, typ_notyp, 628, 'ii');
  AddUserIdent(elx_Globals, 'mysql_escape_string           ', obj_function,  typ_strngs,629, 's');
  AddUserIdent(elx_Globals, 'mysql_debug                   ', obj_procedure, typ_notyp, 630, 's');
  AddUserIdent(elx_Globals, 'mysql_fetch_field_direct      ', obj_procedure, typ_notyp, 631, 'iiT');
  AddUserIdent(elx_Globals, 'mysql_fetch_fields            ', obj_procedure, typ_notyp, 632, 'iT');
  AddUserIdent(elx_Globals, 'mysql_row_tell                ', obj_procedure, typ_notyp, 633, 'iT');
  AddUserIdent(elx_Globals, 'mysql_connect                 ', obj_function,  typ_ints,  634, 'isss');
  AddUserIdent(elx_Globals, 'mysql_real_connect            ', obj_function,  typ_ints,  635, 'issssisi');
  AddUserIdent(elx_Globals, 'mysql_list_dbs                ', obj_function,  typ_ints,  636, 'IT');
  AddUserIdent(elx_Globals, 'mysql_list_tables             ', obj_function,  typ_ints,  637, 'IT');
  AddUserIdent(elx_Globals, 'mysql_list_fields             ', obj_function,  typ_ints,  638, 'IT');
  AddUserIdent(elx_Globals, 'mysql_list_processes          ', obj_function,  typ_ints,  639, 'I');
  AddUserIdent(elx_Globals, 'mysql_store_result            ', obj_function,  typ_ints,  640, 'I');
  AddUserIdent(elx_Globals, 'mysql_use_result              ', obj_function,  typ_ints,  641, 'I');
  AddUserIdent(elx_Globals, 'mysql_row_seek                ', obj_function,  typ_ints,  642, 'Ii');
  AddUserIdent(elx_Globals, 'mysql_field_seek              ', obj_function,  typ_ints,  643, 'Ii');
  AddUserIdent(elx_Globals, 'mysql_fetch_row               ', obj_function,  typ_ints,  644, 'IT');
  AddUserIdent(elx_Globals, 'mysql_fetch_lengths           ', obj_procedure, typ_notyp, 645, 'IT');
  AddUserIdent(elx_Globals, 'mysql_fetch_field             ', obj_procedure, typ_notyp, 646, 'IT');
  AddUserIdent(elx_Globals, 'mysql_fetch_array             ', obj_function,  typ_ints,  647, 'IT');
  AddUserIdent(elx_Globals, 'mysql_init                    ', obj_function,  typ_ints,  648, 'I');
  {$ENDIF}
  AddUserIdent(elx_Globals, '                              ', obj_procedure, typ_notyp, 649, '');
end; { proc. PartThree }

begin
  SaveOnlyParamList := elx_Globals.elx_OnlyParamList;
  elx_Globals.elx_OnlyParamList := OnlyParamList;

  PartOne;
  PartTwo;
  PartThree;

  elx_Globals.elx_OnlyParamList := SaveOnlyParamList;
end; { proc. elx_AddUserFuncs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ElxModName(FName: String): String;
begin
  ElxModName := NoExtension(FName) + '.elm';
end; { func. ElxModName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor ArrayHolderObj.Init;
begin
  FillChar(ArrayInf, SizeOf(ArrayInf), #0);
  {inherited;}
end; { constructor ArrayHolderObj }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor ArrayHolderObj.Done;
begin
  {inherited;}
end; { destructor Done }

end. { elx_glob }
