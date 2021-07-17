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

  {About semantic versioning at https://semver.org/ }
  TSemanticVersion = packed record
    Major, Minor, Path, Revision: byte;
  end;

  TPluginCoordinates = packed record
    GroupId, ArtifactId: string;
  end;

  TPluginInformation = packed record
    Coordinates: TPluginCoordinates;
    Version: TSemanticVersion;
  end;

  {Build system information using for project}
  TBuildSystemInformation = packed record
    {build system version}
    Version: TSemanticVersion;
    {build system type}
    BSType: TBuildSystemType;
    {If use plugin required, fill this property instead of @link(PathToTool)}
    PluginInformation: TPluginInformation;
    {Path to build tool (f.e. ant, predefined version of gradle, maven). Don't use it in new projects! @deprecated }
    PathToTool: string;
  end;

  {Android manifest API version information}
  TApiInformation = packed record
    MinApiVersion, TargetApiVersion, CompileApiVersion: integer;
  end;

  TGeneratedSourceInformation = packed record
    PackageName: string;
    ThemeName: string;
    IsAppCompat: boolean;
  end;

  TCompilerArchitecture = packed record
    InstructionSet: string;    {ArmV6}
    FPUSet: string;            {Soft}
  end;

  TNdkInformation = packed record
    NdkVersion: TSemanticVersion;
    NdkVersionName: string;
    PathToNdk: string;
  end;

  TSdkInformation = packed record
    BuildToolsVersion: string;
    PathToSdk: string;
  end;

  TJdkInformation = packed record
    Version: TSemanticVersion;
    PathToJdk: string;
  end;

  TProjectInformation = packed record
    Name: string;
    CompilerArchitecture: TCompilerArchitecture;
    BuildSystemInformation: TBuildSystemInformation;
    GeneratedSourceInformation: TGeneratedSourceInformation;
    ApiInformation: TApiInformation;
    Version: TSemanticVersion;
    VersionName: string;
    SyntaxMode: TSyntaxMode;
    ModuleType: TModuleType;
  end;

implementation

end.
