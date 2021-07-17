{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazAndroidWizardPack;

{$warn 5023 off : no warning about unused units}
interface

uses
  registerPackageUnit, android.wizard.common.types, android.wizard.project.descriptor.main, android.wizard.project.descriptor.gui, ProjectWorkspaceMainForm, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registerPackageUnit', @registerPackageUnit.Register);
end;

initialization
  RegisterPackage('LazAndroidWizardPack', @Register);
end.
