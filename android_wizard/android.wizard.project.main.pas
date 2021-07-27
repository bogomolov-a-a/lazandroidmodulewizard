unit Android.Wizard.Project.Main;

{$mode objfpc}{$H+}

interface

uses
  android.wizard.common.types;

type

  IModuleDependentFileBuilder = interface
    function GetModuleType(): TModuleType;
  end;

  IBuildSystemBuildFileBuilder = interface(IModuleDependentFileBuilder)

    function BuildAntProjectBuildXml(AProjectInformation: TProjectInformation): boolean;
    function BuildGradleBuildGradleFile(AProjectInformation: TProjectInformation): boolean;
  end;

  { TBuildSystemBuildFileBuilderFactory }

  TBuildSystemBuildFileBuilderFactory = class
    class function CreateBuilder(AModuleType: TModuleType): IBuildSystemBuildFileBuilder;
  end;

  IAndroidManifestFileBuilder = interface(IModuleDependentFileBuilder)
    function BuildManifest(AndroidProjectName: string; PackageName: string;
      AppActivityClassName: string; MinSdkVersion: integer;
      TargetSdkVersion: integer): boolean;
  end;

  { TAndroidManifestBuilderFactory }

  TAndroidManifestBuilderFactory = class
    class function CreateBuilder(moduleType: TModuleType): IAndroidManifestFileBuilder;
  end;

{
Returns full path for 'lamw' package resource
}
function GetAbsolutePathForPackageResouces(RelativePath: string): string;

implementation

uses
  Classes, SysUtils, PackageIntf;

const
  ANDROID_MANIFIST_XML_FILE_NAME = 'AndroidManifest.xml';
{ TBuildSystemBuildFileBuilder }
type

  { TModuleDependentFileBuilder }

  TModuleDependentFileBuilder = class(TInterfacedObject, IModuleDependentFileBuilder)
  strict private
    FAppModuleType: TModuleType;
    FResourceLocationPath: string;

  protected
    function GetModuleType: TModuleType;
    property AppModuleType: TModuleType read GetModuleType;
    property ResourceLocationPath: string read FResourceLocationPath;

  public
    constructor Create(AAppModuleType: TModuleType);
  end;

  TBuildSystemBuildFileBuilder = class(TModuleDependentFileBuilder,
    IBuildSystemBuildFileBuilder)
  strict private
    FBasicTemplateDirectoryPath: string;
  protected
    property BasicTemplateDirectoryPath: string
      read FBasicTemplateDirectoryPath write FBasicTemplateDirectoryPath;
  public
    function BuildAntProjectBuildXml(AProjectInformation: TProjectInformation): boolean;
    function BuildGradleBuildGradleFile(AProjectInformation: TProjectInformation): boolean;
  end;

  { TAndroidManifestBuilder }

  TAndroidManifestBuilder = class(TModuleDependentFileBuilder,
    IAndroidManifestFileBuilder)
  strict private
  const
    DUMMY_PACKAGE_LOCATION: string = 'dummyPackage';
    DUMMY_APPNAME_LOCATION: string = 'dummyAppName';
    DUMMY_MIN_SDK_API_LOCATION: string = 'dummySdkApi';
    DUMMY_TARGET_SDK_API_LOCATION: string = 'dummyTargetApi';
  public
    function BuildManifest(AndroidProjectName: string; PackageName: string;
      AppActivityClassName: string; MinSdkVersion: integer;
      TargetSdkVersion: integer): boolean;
  end;

function GetAbsolutePathForPackageResouces(RelativePath: string): string;
var
  Pkg: TIDEPackage;
begin
  Result := '';
  Pkg := PackageEditingInterface.FindPackageWithName('lazandroidwizardpack');
  if Pkg <> nil then
  begin
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(pkg.Filename)) + RelativePath;
  end;
end;

{ TModuleDependentFileBuilder }

function TModuleDependentFileBuilder.GetModuleType: TModuleType;
begin
  Result := FAppModuleType;
end;

constructor TModuleDependentFileBuilder.Create(AAppModuleType: TModuleType);
begin
  FAppModuleType := AAppModuleType;
  FResourceLocationPath := GetAbsolutePathForPackageResouces(
    SubDirectoryByModuleTypeArray[FAppModuleType]);
end;

{ TAndroidManifestBuilder }

function TAndroidManifestBuilder.BuildManifest(AndroidProjectName: string;
  PackageName: string; AppActivityClassName: string; MinSdkVersion: integer;
  TargetSdkVersion: integer): boolean;
var
  strAfterReplace: string;
begin
  Result := False;
  with TStringList.Create do
    try
      LoadFromFile(IncludeLeadingPathDelimiter(ResourceLocationPath) +
        ANDROID_MANIFIST_XML_FILE_NAME);
      strAfterReplace := StringReplace(Text, DUMMY_PACKAGE_LOCATION,
        PackageName, [rfReplaceAll, rfIgnoreCase]);
      strAfterReplace := StringReplace(strAfterReplace, DUMMY_APPNAME_LOCATION,
        PackageName + '.' + AppActivityClassName, [rfReplaceAll, rfIgnoreCase]);

      strAfterReplace := StringReplace(strAfterReplace, DUMMY_MIN_SDK_API_LOCATION,
        IntToStr(MinSdkVersion), [rfReplaceAll, rfIgnoreCase]);
      strAfterReplace := StringReplace(strAfterReplace,
        DUMMY_TARGET_SDK_API_LOCATION, IntToStr(TargetSdkVersion),
        [rfReplaceAll, rfIgnoreCase]);
      Clear;
      Text := strAfterReplace;
      SaveToFile(IncludeTrailingPathDelimiter(AndroidProjectName) +
        ANDROID_MANIFIST_XML_FILE_NAME);
      Result := True;
    finally
      Free;
    end;
end;

{ TAndroidManifestBuilderFactory }

class function TAndroidManifestBuilderFactory.CreateBuilder(
  moduleType: TModuleType): IAndroidManifestFileBuilder;
begin
  Result := TAndroidManifestBuilder.Create(moduleType);
end;

function TBuildSystemBuildFileBuilder.BuildAntProjectBuildXml(
 AProjectInformation: TProjectInformation): boolean;
begin
  //deprecated
end;

function TBuildSystemBuildFileBuilder.BuildGradleBuildGradleFile(
  AProjectInformation: TProjectInformation): boolean;
begin
  Result := False;
end;


{ TBuildSystemBuildFileBuilder }


{ TBuildSystemBuildFileBuilderFactory }

class function TBuildSystemBuildFileBuilderFactory.CreateBuilder(
  AModuleType: TModuleType): IBuildSystemBuildFileBuilder;
begin

end;
end.
