unit android.wizard.project.descriptor.gui;

{$mode objfpc}{$H+}

interface

type

  { TGUIAndroidModuleFactory }

  TGUIAndroidModuleFactory = class
  public
    class procedure RegisterGUIAndroidModuleResources();
  end;

const
  GUI_ANDROID_ACTIVITY_FORM_NAME = 'LAMW GUI App main activity form';

implementation

uses
  AndroidWidget,
  FormEditingIntf,
  ProjectIntf,
  android.wizard.project.descriptor.main;

type
  TGUIAndroidModule = class(jForm)            //support to Android Bridges [components]
  end;

  { TGUIAndroidFileDescPascalUnitWithResource }

  TGUIAndroidFileDescPascalUnitWithResource = class(TFileDescPascalUnitWithResource)
    constructor Create; override;

    function CreateSource(const Filename: string; const SourceName: string;
      const ResourceName: string): string; override;

    function GetInterfaceUsesSection: string; override;

    function GetInterfaceSource(const Filename: string;
      const SourceName: string; const ResourceName: string): string; override;

    function GetResourceType: TResourceType; override;
    function GetImplementationSource(const Filename: string;
      const SourceName: string; const ResourceName: string): string; override;
  end;

{ TGUIAndroidFileDescPascalUnitWithResource }

constructor TGUIAndroidFileDescPascalUnitWithResource.Create;
begin
  inherited Create;
  Name := GUI_ANDROID_ACTIVITY_FORM_NAME;
  ResourceClass := TGUIAndroidModule;
  UseCreateFormStatements := True;
end;

function TGUIAndroidFileDescPascalUnitWithResource.CreateSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
begin
  Result := inherited CreateSource(Filename, SourceName, ResourceName);
end;

function TGUIAndroidFileDescPascalUnitWithResource.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
end;

function TGUIAndroidFileDescPascalUnitWithResource.GetInterfaceSource(
  const Filename: string; const SourceName: string; const ResourceName: string): string;
begin
  Result := inherited GetInterfaceSource(Filename, SourceName, ResourceName);
end;

function TGUIAndroidFileDescPascalUnitWithResource.GetResourceType: TResourceType;
begin
  Result := rtRes;
end;

function TGUIAndroidFileDescPascalUnitWithResource.GetImplementationSource(
  const Filename: string; const SourceName: string; const ResourceName: string): string;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
end;

{ TGUIAndroidModuleFactory }

class procedure TGUIAndroidModuleFactory.RegisterGUIAndroidModuleResources();
var
  GUIAndroidFileDescriptor: TGUIAndroidFileDescPascalUnitWithResource;
begin
  GUIAndroidFileDescriptor := TGUIAndroidFileDescPascalUnitWithResource.Create;
  RegisterProjectFileDescriptor(GUIAndroidFileDescriptor, PROJECT_LAMW_CATEGORY_NAME);
  FormEditingHook.RegisterDesignerBaseClass(TGUIAndroidModule);
end;

end.
