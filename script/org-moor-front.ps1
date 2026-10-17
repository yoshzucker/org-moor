<#
.SYNOPSIS
  Take the document in front into Emacs, once.

.DESCRIPTION
  Run from a global hotkey while the document is in front: a file open in
  PowerPoint, Word or Excel, a file selected in Explorer, or a page in Edge
  Chrome, Firefox or Zen.  It asks the app what it has open, hands that to Emacs through
  org-protocol, and exits; nothing stays running.

  A document opened from the web -- SharePoint, OneDrive -- has a URL for
  a file, which nothing outside the app can download without signing in.
  The app itself can: it is asked to save a copy, which is what is taken,
  with the URL kept as where it came from.

  The URL is handed to emacsclientw directly: $env:EMACSCLIENT, else the
  one on PATH.  A Scoop shim on PATH is followed to the program it stands
  for, since a shim is a console program and would show a window.  Only
  when there is no emacsclientw is the org-protocol URL opened through
  Windows, which then needs the scheme registered.

.PARAMETER Hwnd
  Window to read.  Without it, the foreground window.
#>
param([long]$Hwnd = 0)
$ErrorActionPreference = 'SilentlyContinue'

Add-Type @"
using System;
using System.Runtime.InteropServices;
public class OrgMoorWin {
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
  [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);
}
"@

$handle = if ($Hwnd -ne 0) { [IntPtr]$Hwnd } else { [OrgMoorWin]::GetForegroundWindow() }
if ($handle -eq [IntPtr]::Zero) { exit 0 }
$procId = 0
[void][OrgMoorWin]::GetWindowThreadProcessId($handle, [ref]$procId)
$proc = Get-Process -Id $procId
$exe = $proc.ProcessName

$copies = Join-Path $env:TEMP 'org-moor'
New-Item -ItemType Directory -Force -Path $copies | Out-Null
# Copies saved for an earlier taking have been taken by now.
Get-ChildItem -LiteralPath $copies -File |
  Where-Object { $_.LastWriteTime -lt (Get-Date).AddDays(-1) } |
  Remove-Item -Force

function Get-OfficeDocument($progId, $property) {
  $app = [Runtime.InteropServices.Marshal]::GetActiveObject($progId)
  if ($app) { return $app.$property }
}

function Get-UrlFileName($url) {
  $path = ([uri]$url).AbsolutePath
  return [uri]::UnescapeDataString(($path -split '/')[-1])
}

function Get-BrowserUrl($h) {
  Add-Type -AssemblyName UIAutomationClient
  Add-Type -AssemblyName UIAutomationTypes
  $root = [System.Windows.Automation.AutomationElement]::FromHandle($h)
  if (-not $root) { return '' }
  $cond = New-Object System.Windows.Automation.PropertyCondition(
    [System.Windows.Automation.AutomationElement]::ControlTypeProperty,
    [System.Windows.Automation.ControlType]::Edit)
  foreach ($e in $root.FindAll([System.Windows.Automation.TreeScope]::Descendants, $cond)) {
    $v = $e.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)
    if ($v -and $v.Current.Value -match '^https?://') { return $v.Current.Value }
    if ($v -and $v.Current.Value -match '^[\w.-]+\.[a-z]{2,}(/|$)') { return 'https://' + $v.Current.Value }
  }
  return ''
}

$path = ''; $url = ''; $origin = ''; $title = $proc.MainWindowTitle

switch -Regex ($exe) {
  '^(POWERPNT|WINWORD|EXCEL)$' {
    $doc = switch ($exe) {
      'POWERPNT' { Get-OfficeDocument 'PowerPoint.Application' 'ActivePresentation' }
      'WINWORD'  { Get-OfficeDocument 'Word.Application' 'ActiveDocument' }
      'EXCEL'    { Get-OfficeDocument 'Excel.Application' 'ActiveWorkbook' }
    }
    if (-not $doc) { break }
    $full = [string]$doc.FullName
    $title = [string]$doc.Name
    if ($full -match '^https?://') {
      $copy = Join-Path $copies (Get-UrlFileName $full)
      # PowerPoint and Excel save a copy and keep working on the original.
      # Word has no such call -- its SaveAs moves the open document onto the
      # copy -- so a Word document from the web is taken as its URL only.
      if ($exe -ne 'WINWORD') {
        try { $doc.SaveCopyAs($copy) } catch {}
      }
      if (Test-Path -LiteralPath $copy) { $path = $copy; $origin = $full }
      else { $url = $full }
    } else {
      $path = $full
    }
  }
  '^explorer$' {
    $shell = New-Object -ComObject Shell.Application
    foreach ($w in $shell.Windows()) {
      if ([long]$w.HWND -eq [long]$handle) {
        $item = @($w.Document.SelectedItems())[0]
        if ($item -and -not $item.IsFolder) { $path = $item.Path; $title = '' }
      }
    }
  }
  '^(msedge|chrome|brave|firefox|zen)$' {
    $url = Get-BrowserUrl $handle
    $title = $title -replace ' - (Microsoft.? Edge|Google Chrome|Brave)$', '' -replace ' - [^-]*Microsoft.? Edge$', '' -replace ' [-\u2014] (Mozilla Firefox|Zen( Browser)?)$', ''
  }
}

if (-not $path -and -not $url) { exit 0 }

$q = if ($path) { 'path=' + [uri]::EscapeDataString($path) } else { 'url=' + [uri]::EscapeDataString($url) }
if ($origin) { $q += '&origin=' + [uri]::EscapeDataString($origin) }
if ($title) { $q += '&title=' + [uri]::EscapeDataString($title) }
$protocolUrl = 'org-protocol://moor?' + $q

function Resolve-EmacsClient {
  if ($env:EMACSCLIENT -and (Test-Path -LiteralPath $env:EMACSCLIENT)) { return $env:EMACSCLIENT }
  $cmd = Get-Command emacsclientw.exe -CommandType Application -ErrorAction SilentlyContinue |
    Select-Object -First 1
  if (-not $cmd) { return '' }
  $exe = $cmd.Source
  $shim = [IO.Path]::ChangeExtension($exe, '.shim')
  if (Test-Path -LiteralPath $shim) {
    $line = Get-Content -LiteralPath $shim | Where-Object { $_ -match '^\s*path\s*=' } | Select-Object -First 1
    if ($line -match '=\s*"?([^"]+?)"?\s*$' -and (Test-Path -LiteralPath $Matches[1])) {
      return $Matches[1]
    }
  }
  return $exe
}

$client = Resolve-EmacsClient
if ($client) {
  Start-Process -FilePath $client -ArgumentList @('-n', ('"' + $protocolUrl + '"')) -WindowStyle Hidden
} else {
  Start-Process $protocolUrl
}
