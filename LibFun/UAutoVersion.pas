{*******************************************************************************
  作者: dmzn@163.com 2019-05-05
  描述: 自动增加版本号

  备注:
  *.版本号更新方式:
    1.MajorVersion: 主版本号保持不变.
    2.MinorVersion: 次版本号保持不变.
    3.Release: 发布版本号为: 月份2位,日期2位,年份1位
    4.Build: 构建版本号为: 构建版本递增.
*******************************************************************************}
program UAutoVersion;

uses
  SysUtils, Classes, CnCommon, CnWizUtils;

var
  gStr: string;
  gOptions: IOTAProjectOptions;
  gYear,gMonth,gDay: Word;
  gMajor,gMinor,gRelease,gBuildNo: Integer;
begin
  if CnOtaGetCurrentProject = nil then Exit;  
  gOptions := CnOtaGetActiveProjectOptions(nil);
  if gOptions = nil then Exit;

  gMajor   := gOptions.GetOptionValue('MajorVersion');
  gMinor   := gOptions.GetOptionValue('MinorVersion');
  gRelease := gOptions.GetOptionValue('Release');
  gBuildNo := gOptions.GetOptionValue('Build');

  CnOtaSetProjectOptionValue(gOptions, 'MajorVersion', Format('%d', [gMajor]));
  CnOtaSetProjectOptionValue(gOptions, 'MinorVersion', Format('%d', [gMinor]));

  DecodeDate(Now(), gYear, gMonth, gDay);
  gStr := Format('%.2d', [gMonth]) + Format('%.2d', [gDay]) +
          Copy(Format('%d', [gYear]), 4, 1);
  CnOtaSetProjectOptionValue(gOptions, 'Release', gStr);

  if gRelease <> StrToInt(gStr) then
       gBuildNo := 1
  else gBuildNo := gBuildNo + 1;
  CnOtaSetProjectOptionValue(gOptions, 'Build', Format('%d', [gBuildNo]));
end.
