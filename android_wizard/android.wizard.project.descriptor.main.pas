unit android.wizard.project.descriptor.main;

{$mode objfpc}{$H+}

interface

uses
  Android.wizard.project.Main,
  Android.wizard.common.types,
  ProjectIntf;

type
  IPascalProjectFileBuilder = interface(IModuleDependentFileBuilder)
    procedure CreateProjectFiles(AProject: TLazProject;
      AProjectInformation: TProjectInformation);
    procedure AddMainFile(AProject: TLazProject;
      AProjectInformation: TProjectInformation);
  end;

  IJavaProjectFileBuilder = interface(IModuleDependentFileBuilder)
    function CreateFiles(AProject: TLazProject;
      AProjectInformation: TProjectInformation): boolean;
  end;

  { TAbstractPascalProjectFileBuilder }

  TAbstractPascalProjectFileBuilder = class abstract (TModuleDependentFileBuilder,
    IPascalProjectFileBuilder)
  strict private
    function GetMainProjectFileName(AProjectInformation: TProjectInformation): string;
    function CreateProjectMainFile(AProject: TLazProject;
      AProjectInformation: TProjectInformation): TLazProjectFile;
    function GenerateMainFileSourceCode(): string;
    function GetProgramHeader(): string; virtual; abstract;
  public
    procedure CreateProjectFiles(AProject: TLazProject;
      AProjectInformation: TProjectInformation);
    procedure AddMainFile(AProject: TLazProject;
      AProjectInformation: TProjectInformation);
  end;

  { TAndroidProjectDescriptorFactory }

  TAndroidProjectDescriptorFactory = class
  public
    class procedure RegisterAndroidProjectDescriptor();
  end;

const
  JNI_PROJECT_PATH: string = 'jni';
  PROJECT_LAMW_CATEGORY_NAME: string = 'Lazarus Android Module Wizard';

implementation

uses
  SysUtils,
  UITypes,
  Forms,
  NewItemIntf,
  LazIDEIntf,
  ProjectWorkspaceMainForm,
  IDEMsgIntf,
  IDEExternToolIntf,
  Classes;

type
  {UPD 12.07.2021
    @author bogomolov-a-a
    add builder for project files
    }

  { TAndroidProjectDescriptor }

  TAndroidProjectDescriptor = class(TProjectDescriptor)
  strict private
    FPascalJNIInterfaceCode: string;
    FPascalProjectFileBuilder: IPascalProjectFileBuilder;
    FJavaProjectFileBuilder: IJavaProjectFileBuilder;
    FManifestFileBuilder: IAndroidManifestFileBuilder;
    FBuildSystemBuildBuilder: IBuildSystemBuildFileBuilder;
    FProjectInformation: TProjectInformation;
  strict private
  const
    PROJECT_LAMW_DESCRIPTOR_NAME: string = 'LAMW Project';
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

{ TAbstractPascalProjectFileBuilder }

function TAbstractPascalProjectFileBuilder.GetMainProjectFileName(
  AProjectInformation: TProjectInformation): string;
begin
  Result := AProjectInformation.Name + '.lpr';
end;

function TAbstractPascalProjectFileBuilder.CreateProjectMainFile(AProject: TLazProject;
  AProjectInformation: TProjectInformation): TLazProjectFile;
var
  projFileName: string;
  resultSourceCode: string;
begin
  projFileName := GetMainProjectFileName(AProjectInformation);
  Result := AProject.CreateProjectFile(IncludeTrailingPathDelimiter(
    AProjectInformation.ProjectPath) + projFileName);
  Result.IsPartOfProject := True;
  resultSourceCode := GenerateMainFileSourceCode();
  {write with formatting}
  Result.SetSourceText(resultSourceCode, True);
end;

function TAbstractPascalProjectFileBuilder.GenerateMainFileSourceCode(): string;
var
  SourceCodeLineList: TStringList;
begin
  SourceCodeLineList := TStringList.Create;
  {Linux syle by default}
  SourceCodeLineList.TextLineBreakStyle := tlbsLF;
  try
    Result := SourceCodeLineList.Text;
    SourceCodeLineList.Append(GetProgramHeader());
  finally
    FreeAndNil(SourceCodeLineList);
  end;

end;

procedure TAbstractPascalProjectFileBuilder.CreateProjectFiles(AProject: TLazProject;
  AProjectInformation: TProjectInformation);
begin

end;

procedure TAbstractPascalProjectFileBuilder.AddMainFile(AProject: TLazProject;
  AProjectInformation: TProjectInformation);
var
  MainFile: TLazProjectFile;
begin
  MainFile := CreateProjectMainFile(AProject, AProjectInformation);
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;
end;

{ TAndroidProjectDescriptorFactory }

class procedure TAndroidProjectDescriptorFactory.RegisterAndroidProjectDescriptor();
var
  Descriptor: TAndroidProjectDescriptor;
begin
  RegisterNewItemCategory(TNewIDEItemCategory.Create(PROJECT_LAMW_CATEGORY_NAME));
  Descriptor := TAndroidProjectDescriptor.Create;
  RegisterProjectDescriptor(Descriptor, PROJECT_LAMW_CATEGORY_NAME);
end;

{ TAndroidProjectDescriptor }

constructor TAndroidProjectDescriptor.Create;
begin
  inherited Create;
  Name := PROJECT_LAMW_DESCRIPTOR_NAME;
  FBuildSystemBuildBuilder := TBuildSystemBuildFileBuilderFactory.CreateBuilder(
    FProjectInformation.ModuleType);
end;

function TAndroidProjectDescriptor.GetLocalizedName: string;
begin
  Result := inherited GetLocalizedName;
end;

function TAndroidProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := inherited GetLocalizedDescription;
end;

function TAndroidProjectDescriptor.DoInitDescriptor: TModalResult;
var
  form: TWorkspaceProjectMainForm;
begin
  form := TWorkspaceProjectMainForm.Create(nil);
  try
    Result := form.ShowModal;
    if Result <> mrOk then
      exit;
    IDEMessagesWindow.AddCustomMessage(TMessageLineUrgency.mluNote,
      'Project created!');
  finally
    FreeAndNil(form);
  end;

end;

function TAndroidProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  MainFile: TLazProjectFile;
begin
  Result := inherited InitProject(AProject);
  AProject.CustomData['ProjectInformation'] := FProjectInformation.ToJsonString();
  FPascalProjectFileBuilder.AddMainFile(AProject, FProjectInformation);
  LazarusIDE.DoSaveProject([]);
  Result := mrOk;
end;

function TAndroidProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result := inherited CreateStartFiles(AProject);
  FPascalProjectFileBuilder.CreateProjectFiles(AProject, FProjectInformation);
  LazarusIDE.DoSaveProject([]);
  Result := mrOk;
end;

end.
