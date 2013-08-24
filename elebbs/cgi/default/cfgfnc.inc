(*
**
** Configuration record information
**
** Created: 08-Aug-2002
** Last update: 08-Aug-2002
**
** (c) 2002 by Maarten Bekers
**
**
*)


procedure LoadWebConfig;
begin
  {-- Initialize the configuration to a default setting, this should be -----}
  {-- replaced with using a real configuration file -------------------------}
  webCfgInf.FrontPageNum := 1000;                         { Frontpage number }
end; { proc. LoadWebConfig }

