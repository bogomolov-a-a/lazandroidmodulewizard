unit android.wizard.project.descriptor.main;

{$mode objfpc}{$H+}

interface

uses
  Android.wizard.project.Main,
  Android.wizard.common.types;

type
  IJniPascalProjectFileBuilder = interface(IModuleDependentFileBuilder)
    function BuildFiles(): boolean;
  end;

  IJavaProjectFileBuilder = interface(IModuleDependentFileBuilder)
    function BuildFiles(): boolean;
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
  ProjectIntf,
  NewItemIntf,
  LazIDEIntf,
  ProjectWorkspaceMainForm,
  IDEMsgIntf,
  IDEExternToolIntf;

type
  {UPD 12.07.2021
    @author bogomolov-a-a
    add builder for project files
    }

  { TAndroidProjectDescriptor }

  TAndroidProjectDescriptor = class(TProjectDescriptor)
  strict private
    FPascalJNIInterfaceCode: string;
    FJniPascalProjectFileBuilder: IJniPascalProjectFileBuilder;
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
begin
  Result := inherited InitProject(AProject);
end;

function TAndroidProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result := inherited CreateStartFiles(AProject);
end;

end.
