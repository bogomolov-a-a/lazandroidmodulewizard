{
Forked from jmpessoa/lazarusandroidmodulewizard on github.

Contains common types for ide tools and project wizard packages.
@author bogomolov-a-a
@created 2021-07-10
@lastmod  2021-07-17
}
unit android.wizard.common.types;

{$mode objfpc}{$H+}

interface

uses Generics.Collections, SysUtils, Classes;

type
  {Application module type: game module, gui app, no gui app or console app
  For backward compatibility mtGDX=-1 and others order by id.
  @author bogomolov-a-a
  @created 2021-07-10
  }
  TModuleType = (
    {Not implement yet}
    mtGDX = -1,
    {Basic android application with visual activity(ies)}
    mtGUI = 0,
    {May be daemons? Not implement yet.}
    mtNOGUI = 1,
    {Native console app. Write on FPC, for execute need 'Termux' and others. }
    mtCONSOLE_EXEC_APP = 2,
    {Native lib, may be used in another application types as 'so'}
    mtGENERIC_LIB = 3);

const
  {
    For separating Java code by application types add specified directories with
    code, resources and others(for example, native declarations).
    @author bogomolov-a-a
    @created 2021-07-10
  }
  SubDirectoryByModuleTypeArray: array [Low(TModuleType)..High(TModuleType)] of string =
    (
    {@link mtGDX}
    'gdx',
    {@link mtGUI}
    'gui',
    {@link mtNOGUI}
    'nogui',
    {@link mtCONSOLE_EXEC_APP}
    'consoleApp',
    {@link mtGENERIC_LIB}
    'genericLib'
    );

type
  {moved from androidwizard_intf.pas}
  TSyntaxMode = (smDelphi, smObjFpc);
  {Build system type for apk creating.}
  TBuildSystemType = (
    {@deprecated Use @link(bstGradle). Don't use this.}
    bstAnt,
    {Actual build system for android application}
    bstGradle,
    {@deprecated Use @link(bstGradle). Don't use this.}
    bstMaven);

const
  SNAPSHOT_VERSION: string = 'SNAPSHOT';

type
  {About semantic versioning at https://semver.org/ }

  { TSemanticVersion }

  TSemanticVersion = class(TPersistent)
  strict private
    FMajor: word;
    FMinor: word;
    FPath: word;
    FRevision: string;
  strict private
  const
    PRINTABLE_FORMAT: string = '%d.%d.%d';
  public
    function ToString: ansistring; override;
    function Equal(y: TSemanticVersion): boolean;
    class function FromString(Version: string): TSemanticVersion;

  published
    property Major: word read FMajor write FMajor;
    property Minor: word read FMinor write FMinor;
    property Path: word read FPath write FPath;
    property Revision: string read FRevision write FRevision;
  end;

  { Build system plugin maven coordinate}
  TPluginCoordinates = class(TPersistent)
  strict private
    {groupId}
    FGroupId: string;
    {artifactId}
    FArtifactId: string;
  strict private
  const
    PRINTABLE_FORMAT: string = '%s:%s';
  public
    function ToString: ansistring; override;
  published
    property GroupId: string read FGroupId write FGroupId;
    property ArtifactId: string read FArtifactId write FArtifactId;
  end;

  {Full plugin coordinate with version}

  { TPluginInformation }

  TPluginInformation = class(TPersistent)
  strict private
    FCoordinates: TPluginCoordinates;
    FVersion: TSemanticVersion;
  strict private
  const
    PRINTABLE_FORMAT: string = '%s:%s';
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: ansistring; override;
  published
    property Coordinates: TPluginCoordinates read FCoordinates write FCoordinates;
    property Version: TSemanticVersion read FVersion write FVersion;
  end;

  {Build system information using for project}

  { TBuildSystemInformation }

  TBuildSystemInformation = class(TPersistent)
  strict private
    {build system version}
    FVersion: TSemanticVersion;
    {build system type}
    FBSType: TBuildSystemType;
    {If use plugin required, fill this property instead of @link(PathToTool)}
    FPluginInformation: TPluginInformation;
    {Path to build tool (f.e. ant, predefined version of gradle, maven). Don't use it in new projects! @deprecated }
    FPathToTool: string;
    procedure SetPluginInformation(AValue: TPluginInformation);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Version: TSemanticVersion read FVersion write FVersion;
    property BSType: TBuildSystemType read FBSType write FBSType;
    property PluginInformation: TPluginInformation
      read FPluginInformation write SetPluginInformation;
    property PathToTool: string read FPathToTool write FPathToTool;
  end;

  {Android manifest API version information}
  TApiInformation = class(TPersistent)
  strict private
    FMinApiVersion: word;
    FTargetApiVersion: word;
    FCompileApiVersion: word;
  published
    property MinApiVersion: word read FMinApiVersion write FMinApiVersion;
    property TargetApiVersion: word read FTargetApiVersion write FTargetApiVersion;
    property CompileApiVersion: word read FCompileApiVersion write FCompileApiVersion;
  end;

  {Information about main package,basic theme}

  { TGeneratedSourceInformation }

  TGeneratedSourceInformation = class(TPersistent)
  strict private
    {Main package name, with app class,used as applicationId in build files }
    FPackageName: string;
    {Main app theme, f.e. NoActionBar}
    FThemeName: string;
    {If ThemeName contains 'AppCompat'}
    FIsAppCompat: boolean;
    function GetIsAppCompat: boolean;
  published
    property PackageName: string read FPackageName write FPackageName;
    property ThemeName: string read FThemeName write FThemeName;
    property IsAppCompat: boolean read GetIsAppCompat write FIsAppCompat;
  end;

  {Architecture cross-compiler, for example arm, instruction set optional}
  TCompilerArchitecture = class(TPersistent)
  strict private
    {by default 'ArmV6'}FInstructionSet: string;
    {by default 'Soft'}FFPUSet: string;
  published
    property InstructionSet: string read FInstructionSet write FInstructionSet;
    property FPUSet: string read FFPUSet write FFPUSet;
  end;

  TDevelepmentKitInformation = class(TPersistent)
  strict private
    {path on user device with development kit}
    FPathToDevelopmentKit: string;
    {download url for specified version from internet}
    FDownloadUrl: string;
  published
    property PathToPathToDevelopmentKit: string
      read FPathToDevelopmentKit write FPathToDevelopmentKit;
    property DownloadLink: string read FDownloadUrl write FDownloadUrl;
  end;

  {Using NDK for JNI project build}

  { TNdkInformation }

  TNdkInformation = class(TDevelepmentKitInformation)
  strict private
    {version, for example 11+}
    FVersion: TSemanticVersion;
    {version name, for example r10e}
    FNdkVersionName: string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMaxNdkPlatformApiVersion: word;
  published
    property Version: TSemanticVersion read FVersion write FVersion;
    property NdkVersionName: string read FNdkVersionName write FNdkVersionName;

  end;

  {Used Android SDK project information}

  { TSdkInformation }

  TSdkInformation = class(TDevelepmentKitInformation)
  strict private
    {Number of platform version of Android SDK, for example '29'}
    FBuildToolsVersionPlatform: word;
    {Full version of Android SDK, for example '29.0.3'}
    FBuildToolsVersionName: string;
    {path on user device with sdk}
  public
    function GetCodeNameByApi: string;
  published
    property BuildToolsVersionPlatform: word
      read FBuildToolsVersionPlatform write FBuildToolsVersionPlatform;
    property BuildToolsVersionName: string read FBuildToolsVersionName
      write FBuildToolsVersionName;
  end;

  {Information about used JDK}

  { TJdkInformation }

  TJdkInformation = class(TPersistent)
  strict private
    {jdk version for source and target compatibility}
    FVersion: TSemanticVersion;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Version: TSemanticVersion read FVersion write FVersion;
  end;

  {Dimension application.
  You can use it for separate application code by device architecture, screen resolution,
  free and payed version and others...}
  TDimension = class(TPersistent)
  strict private
    {dimension name, f.e. 'version' for demo,free,payed version}
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  TDimensionList = specialize TList<TDimension>;
  {Product flavor can used for separate application code by device architecture, screen resolution,
  free and payed version and others. For each flavor must be specified his dimension.}
  TProductFlavor = class(TPersistent)
  strict private
    {Flavor name}
    FName: string;
    {Relative dimension name}
    FDimensionName: string;
    {applicationId suffix}
    FApplicationIdSuffix: string;
    {version name suffix}
    FVersionNameSuffix: string;
  published
    property Name: string read FName write FName;
    property DimensionName: string read FDimensionName write FDimensionName;
    property ApplicationIdSuffix: string read FApplicationIdSuffix
      write FApplicationIdSuffix;
    property VersionNameSuffix: string read FVersionNameSuffix write FVersionNameSuffix;
  end;

  TProductFlavorList = specialize TList<TProductFlavor>;
  {Application build type (f.e. debug,release... )}
  TBuildType = class(TPersistent)
  strict private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  TBuildTypeList = specialize TList<TBuildType>;

  TBuildMode = class(TPersistent)
  strict private
    FProductFlavorName: string;
    FBuildTypeName: string;
  published
    property ProductFlavorName: string read FProductFlavorName write FProductFlavorName;
    property BuildTypeName: string read FBuildTypeName write FBuildTypeName;
  end;

  TBuildModeList = specialize TList<TBuildMode>;
  { TProjectInformation }

  TProjectInformation = class(TPersistent)
  strict private
    // project info

    FName: string;
    FProjectPath: string;
    FVersion: TSemanticVersion;
    FVersionName: string;
    FSyntaxMode: TSyntaxMode;
    FModuleType: TModuleType;

    //compiler info

    FCompilerArchitecture: TCompilerArchitecture;

    //build system info

    FBuildSystemInformation: TBuildSystemInformation;

    //java code information
    FGeneratedSourceInformation: TGeneratedSourceInformation;
    // api versions
    FApiInformation: TApiInformation;

    // build types and flavors
    FProductFlavors: TProductFlavorList;
    FBuildTypes: TBuildTypeList;
    FBuildModes: TBuildModeList;

    // development kit informations
    FJdkInformation: TJdkInformation;
    FNdkInformation: TNdkInformation;
    FSdkInformation: TSdkInformation;
    function GetProjectPath: string;
  public
    constructor Create;
    destructor Destroy; override;
    function ToJsonString(): string;
    class function FromJsonString(Data: string): TProjectInformation;
    function IsAndroidApplication(): boolean;
  published
    property Name: string read FName write FName;
    property ProjectPath: string read GetProjectPath write FProjectPath;
    property Version: TSemanticVersion read FVersion write FVersion;
    property VersionName: string read FVersionName write FVersionName;
    property SyntaxMode: TSyntaxMode read FSyntaxMode write FSyntaxMode;
    property ModuleType: TModuleType read FModuleType write FModuleType;

    //compiler info

    property CompilerArchitecture: TCompilerArchitecture
      read FCompilerArchitecture write FCompilerArchitecture;

    //build system info

    property BuildSystemInformation: TBuildSystemInformation
      read FBuildSystemInformation write FBuildSystemInformation;

    //java code information
    property GeneratedSourceInformation: TGeneratedSourceInformation
      read FGeneratedSourceInformation write FGeneratedSourceInformation;
    // api versions
    property ApiInformation: TApiInformation read FApiInformation write FApiInformation;

    // build types and flavors
    property ProductFlavors: TProductFlavorList read FProductFlavors;
    property BuildTypes: TBuildTypeList read FBuildTypes;
    property BuildModes: TBuildModeList read FBuildModes;

    // development kit informations
    property JdkInformation: TJdkInformation read FJdkInformation write FJdkInformation;
    property NdkInformation: TNdkInformation read FNdkInformation write FNdkInformation;
    property SdkInformation: TSdkInformation read FSdkInformation write FSdkInformation;
  end;

  { TDevelopmentKits }

  TDevelopmentKits = class
    abstract
  strict private
    FPathToKit: string;
  strict protected
    property PathToKit: string read FPathToKit;
    function GetRelativePathToKit(): string; virtual; abstract;
  public
    constructor Create;
    function GetDevelopmentKitVersions(out VersionList: TStringList): boolean;
  end;

  { TSdks }

  TSdks = class(TDevelopmentKits)
  strict private
    function HasBuildTools(platform: integer; out outBuildTool: string): boolean;
  strict protected
    function GetRelativePathToKit(): string; override;
  public
  const
    PATH_TO_KIT = '.androidSdks';
    BUIlD_TOOLS_DIR = 'build-tools';
    ANT_TOOL_DIRECTORY = 'tools' + PathDelim + 'ant';
  public
    function GetMaxSdkPlatform(): word;
    function IsAntEnable: boolean;
  end;

  { TNdks }

  TNdks = class(TDevelopmentKits)
  strict protected
    function GetRelativePathToKit(): string; override;
  public
  const
    PATH_TO_KIT = '.ndks';
    TOOLCHAIN_DIRECTORY =
      'toolchains' + PathDelim + 'arm-linux-androideabi-4.9' +
      PathDelim + 'prebuilt' + PathDelim;
  public
    function GetPrebuiltDirectory: string;

  end;

  { TJdks }

  TJdks = class(TDevelopmentKits)
  strict protected
    function GetRelativePathToKit(): string; override;
  public
  const
    PATH_TO_KIT = '.jdks';
  end;


  EProjectInformationBuildException = class(Exception)

  end;

resourcestring
  PARAMETER_NOT_SPECIFIED = ' %s can''t be empty!';

var
  {Global object with information about all installed in system Android SDK}
  Sdks: TSdks;
  {Global object with information about all installed in system Andorid NDK}
  Ndks: TNdks;
  {Global object with information about all installed in system JDK}
  Jdks: TJdks;

implementation

uses fpjsonrtti, FileUtil;

{ TJdks }

function TJdks.GetRelativePathToKit(): string;
begin
  Result := PATH_TO_KIT;
end;

{ TNdks }

function TNdks.GetRelativePathToKit(): string;
begin
  Result := PATH_TO_KIT;
end;

function TNdks.GetPrebuiltDirectory: string;
var
  pathToNdkToolchains49: string;  //   [ARM or x86]
begin
  Result := '';
  pathToNdkToolchains49 := IncludeTrailingPathDelimiter(PathToKit) + TOOLCHAIN_DIRECTORY;
    {$ifdef windows}
     Result:=  'windows';
     if DirectoryExists(pathToNdkToolchains49+ 'windows-x86_64') then Result:= 'windows-x86_64';
   {$else}
     {$ifdef darwin}
        Result:=  '';
        if DirectoryExists(pathToNdkToolchains49+ 'darwin-x86_64') then Result:= 'darwin-x86_64';
     {$else}
       {$ifdef linux}
         Result:=  'linux-x86_32';
         if DirectoryExists(pathToNdkToolchains49+ 'linux-x86_64') then Result:= 'linux-x86_64';
       {$endif}
     {$endif}
   {$endif}

  if Result = '' then
  begin
       {$ifdef WINDOWS}
         Result:= 'windows-x86_64';
       {$endif}
       {$ifdef LINUX}
           Result:= 'linux-x86_64';
       {$endif}
       {$ifdef darwin}
           Result:= 'darwin-x86_64';
       {$endif}
  end;

end;

{ TDevelopmentKits }

constructor TDevelopmentKits.Create;
begin
  FPathToKit := IncludeTrailingPathDelimiter(GetUserDir) + GetRelativePathToKit();
end;

function TDevelopmentKits.GetDevelopmentKitVersions(
  out VersionList: TStringList): boolean;
begin
  Result := False;
end;

{ TSdkInformation }

function TSdkInformation.GetCodeNameByApi: string;
begin
  case FBuildToolsVersionPlatform of
    13:
      Result := 'Honeycomb 3.2';
    14:
      Result := 'IceCream 4.0';
    15:
      Result := 'IceCream 4.0x';
    16:
      Result := 'JellyBean 4.1';
    17:
      Result := 'JellyBean 4.2';
    18:
      Result := 'JellyBean 4.3';
    19:
      Result := 'KitKat 4.4';
    20:
      Result := 'KitKat 4.4x';
    21:
      Result := 'Lollipop 5.0';
    22:
      Result := 'Lollipop 5.1';
    23:
      Result := 'Marshmallow 6.0';
    24:
      Result := 'Nougat 7.0';
    25:
      Result := 'Nougat 7.1';
    26:
      Result := 'Oreo 8.0';
    27:
      Result := 'Oreo 8.1';
    28:
      Result := 'Pie 9.0';
    29:
      Result := 'Android 10';
    30:
      Result := 'Android 11';
    else
      Result := 'Unknown';
  end;
end;

{ TSdks }

function TSdks.GetRelativePathToKit(): string;
begin
  Result := PATH_TO_KIT;
end;

function TSdks.GetMaxSdkPlatform(): word;
var
  lisDir: TStringList;
  strApi: string;
  i, intApi: integer;
  outBuildTool: string;
  CandidateSdkPlatform: word;
begin
  Result := 0;
  CandidateSdkPlatform := 0;

  lisDir := TStringList.Create;
  FindAllDirectories(lisDir, IncludeTrailingPathDelimiter(PathToKit) +
    'platforms', False);
  try
    if lisDir.Count = 0 then
      exit;

    for i := 0 to lisDir.Count - 1 do
    begin
      strApi := ExtractFileName(lisDir.Strings[i]);
      if strApi <> '' then
      begin
        strApi := Copy(strApi, LastDelimiter('-', strApi) + 1, MaxInt);
        if TryStrToInt(strApi, intApi) then  {
        skip last version android api,trying convert string to int.
         }
        begin
          if CandidateSdkPlatform < intApi then
            CandidateSdkPlatform := intApi;
          if Result < intApi then
          begin
            CandidateSdkPlatform := intApi;
            if HasBuildTools(intApi, outBuildTool) then
              Result := intApi;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(lisDir);
  end;
end;

function TSdks.IsAntEnable: boolean;
begin
  Result := False;
  if DirectoryExists(IncludeTrailingPathDelimiter(PathToKit) + ANT_TOOL_DIRECTORY) then
  begin
    Result := True;
  end;
end;

function TSdks.HasBuildTools(platform: integer; out outBuildTool: string): boolean;
var
  lisDir: TStringList;
  auxStr: string;
  i: integer;
  semanticVersion: TSemanticVersion;
begin
  Result := False;
  lisDir := TStringList.Create;   //C:\adt32\sdk\build-tools\19.1.0
  try
    FindAllDirectories(lisDir, IncludeTrailingPathDelimiter(PathToKit) +
      BUIlD_TOOLS_DIR, False);
    if lisDir.Count = 0 then
      exit;
    for i := 0 to lisDir.Count - 1 do
    begin
      auxStr := ExtractFileName(lisDir.Strings[i]);
      lisDir.Strings[i] := auxStr;
    end;
    lisDir.Sorted := True;
    for i := 0 to lisDir.Count - 1 do
    begin
      auxStr := lisDir.Strings[i].Trim;
      if auxStr <> '' then    //19.1.0
      begin
        semanticVersion := TSemanticVersion.FromString(auxStr);
        //major:19,minor:1,path:0
        if platform <= semanticVersion.Major then
        begin
          outBuildTool := auxStr; //25.0.3
          Result := True;
        end;
        FreeAndNil(semanticVersion);
        if (Result) then
          break;
      end;
    end;
  finally
    FreeAndNil(lisDir);
  end;
end;

{ TNdkInformation }

constructor TNdkInformation.Create;
begin
  FVersion := TSemanticVersion.Create;
end;

destructor TNdkInformation.Destroy;
begin
  FreeAndNil(FVersion);
  inherited Destroy;
end;

function TNdkInformation.GetMaxNdkPlatformApiVersion: word;
begin
  case FVersion.Major of
    10: Result := 21;
    11: Result := 24;
    12: Result := 24;
    13: Result := 24;
    14: Result := 24;
    15: Result := 26;
    16: Result := 27;
    17: Result := 28;
    18: Result := 28;
    19: Result := 28;
    20: Result := 29;
    21: Result := 30;
    22: Result := 30; //The deprecated "platforms" directories have been removed....
    23: Result := 30;
  end;
end;

{ TGeneratedSourceInformation }

function TGeneratedSourceInformation.GetIsAppCompat: boolean;
begin
  Result := FIsAppCompat or FThemeName.Contains('AppCompat');
end;

{ TJdkInformation }

constructor TJdkInformation.Create;
begin
  FVersion := TSemanticVersion.Create;
end;

destructor TJdkInformation.Destroy;
begin
  FreeAndNil(FVersion);
  inherited Destroy;
end;

{ TProjectInformation }

function TProjectInformation.GetProjectPath: string;
begin
  Result := FProjectPath;
  if (ModuleType = mtGUI) or (ModuleType = mtGDX) then
    Result := Result + 'jni';
end;

constructor TProjectInformation.Create;
begin

end;

destructor TProjectInformation.Destroy;
begin
  inherited Destroy;
end;

function TProjectInformation.ToJsonString(): string;
var
  streamer: TJSONStreamer;
begin
  streamer := TJSONStreamer.Create(nil);
  try
    Result := streamer.ObjectToJSONString(self);
  finally
    FreeAndNil(streamer);
  end;
end;

class function TProjectInformation.FromJsonString(Data: string): TProjectInformation;
var
  destreamer: TJSONDeStreamer;
begin
  Result := TProjectInformation.Create;
  destreamer := TJSONDeStreamer.Create(nil);
  try
    destreamer.JSONToObject(Data, Result);
  finally
    FreeAndNil(destreamer);
  end;
end;

function TProjectInformation.IsAndroidApplication(): boolean;
begin
  Result := (FModuleType = mtGDX) or (FModuleType = mtGUI);
end;

{ TBuildSystemInformation }

procedure TBuildSystemInformation.SetPluginInformation(AValue: TPluginInformation);
begin
  if FPluginInformation = AValue then
    Exit;
  FreeAndNil(FPluginInformation);
  FPluginInformation := AValue;
end;

constructor TBuildSystemInformation.Create;
begin
  FBSType := bstGradle;
  FVersion := TSemanticVersion.Create;
  FPluginInformation := TPluginInformation.Create;
  FVersion.Major := 7;
end;


destructor TBuildSystemInformation.Destroy;
begin
  FPathToTool := '';
  FreeAndNil(FPluginInformation);
  FreeAndNil(FVersion);
  inherited Destroy;
end;

{ TPluginInformation }

constructor TPluginInformation.Create;
begin
  FCoordinates := TPluginCoordinates.Create();
  FVersion := TSemanticVersion.Create;
end;

destructor TPluginInformation.Destroy;
begin
  FreeAndNil(FVersion);
  FreeAndNil(FCoordinates);
  inherited Destroy;
end;

function TPluginInformation.ToString: ansistring;
begin
  Result := format(PRINTABLE_FORMAT, [FCoordinates.ToString, FVersion.ToString]);
end;

{ TPluginCoordinates }

function TPluginCoordinates.ToString: ansistring;
begin
  Result := format(PRINTABLE_FORMAT, [FGroupId, FArtifactId]);
end;

{ TSemanticVersion }

function TSemanticVersion.ToString: ansistring;
begin
  Result := format(PRINTABLE_FORMAT, [FMajor, FMinor, FPath]);
  if trim(FRevision) <> '' then
    Result := Result + '-' + FRevision;
end;

function TSemanticVersion.Equal(y: TSemanticVersion): boolean;
begin
  Result := False;
  if (FMajor <> y.Major) then
    exit;
  if (FMinor <> y.Minor) then
    exit;
  if (FPath <> y.Path) then
    exit;
  if (FRevision <> y.Revision) then
    exit;
  Result := True;
end;

class function TSemanticVersion.FromString(Version: string): TSemanticVersion;
var
  parts: TStringArray;
  AMajor, AMinor, APath: longint;
begin
  Result := nil;
  parts := Version.Split(['.']);
  if (length(parts) > 3) then
    raise EIntError.Create('Invalid semantic version string');
  Result := TSemanticVersion.Create;
  if not TryStrToInt(parts[0], Amajor) then
    raise EIntError.Create('Not valid major version');
  Result.Major := Amajor;
  if not TryStrToInt(parts[1], Aminor) then
    raise EIntError.Create('Not valid minor version');
  Result.Minor := Aminor;
  if not TryStrToInt(parts[2], Apath) then
    raise EIntError.Create('Not valid path version');
  Result.Path := Apath;
end;

initialization
  Sdks := TSdks.Create;
  Ndks := TNdks.Create;
  Jdks := TJdks.Create;

finalization
  FreeAndNil(Jdks);
  FreeAndNil(Ndks);
  FreeAndNil(Sdks);
end.
