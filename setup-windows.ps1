# Not actually meant for running as an invoked script, but rather as a sequence
# of steps to consult and copy and paste into the terminal, and to do some
# unscripted steps manually.

#
# UI Controls
#

# Enable keyboard cues/underlines for accelerators
Set-ItemProperty -Path "HKCU:\Control Panel\Accessibility\Keyboard Preference" -Name "On" -Value "1"

# Show underlined access keys always (not just when Alt is pressed)
Set-ItemProperty -Path "HKCU:\Control Panel\Desktop" -Name "UserPreferencesMask" -Value ([byte[]](0x9E,0x1E,0x07,0x80,0x12,0x00,0x00,0x00))

#
# Appearance
#

# Enable Dark mode for apps
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize" -Name "AppsUseLightTheme" -Value 0

# Enable Dark mode for system UI elements
Set-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize" -Name "SystemUsesLightTheme" -Value 0

Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression
scoop bucket add extras

scoop install steam epic-games-launcher vlc googlechrome wiztree
