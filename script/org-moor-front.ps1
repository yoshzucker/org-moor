<#
.SYNOPSIS
  Take the document in front into Emacs, once.

.DESCRIPTION
  Run from a global hotkey while the document is in front: a file open in
  PowerPoint, Word or Excel, a message in Outlook, a file selected in
  Explorer, or a page in Edge, Chrome, Firefox or Zen.  It asks the app
  what it has open, hands that to Emacs through org-protocol, and exits;
  nothing stays running.

  A document opened from the web -- from a document library -- has a URL
  for a file, which nothing outside the app can download without signing in.
  The app itself can: it is asked to save a copy, which is what is taken,
  with the URL kept as where it came from.

  A file opened in a browser is taken as a file: one on disk by its path,
  and one behind a sign-in -- PowerPoint or Word for the web, a PDF in a
  document library's viewer, a wiki page its site exports as a PDF, a page
  only SingleFile can keep -- by having the browser, which is signed in,
  keep it: it is sent to a URL that downloads it, or sent the keys of an
  extension that saves the page.  Emacs is not kept waiting for that: it
  is told the file is arriving in the Downloads folder, and takes it from
  there once whole, with its URL kept as where it came from.  Any other
  URL Emacs fetches itself.

  Which sites those are is the configuration's, a JSON file at
  ~/.config/org-moor/front.json ($env:XDG_CONFIG_HOME when set): under
  sites, an entry -- one { } -- each, its match a regular expression for
  the URL, and either download, the URL to send the browser to, {name} in
  it the group of that name in match, or keys, what to type in the
  browser, in SendKeys' notation -- ^+y is Ctrl+Shift+Y; and title, when
  given, a regular expression for what the site adds to the window's
  title, which is taken off.  A URL gets the first entry whose match fits
  it, and nothing from any other.  Without
  it, org-moor-front.json beside this script is read, which is also the
  template:

    org-moor-front.ps1 -WriteConfig

  writes it there to be changed, and leaves one already there alone.

  From a hotkey, start it without a profile, which is quickest, and give
  it the window in front, read before PowerShell starts so that its
  console is never what is read.  With AutoHotkey:

    ^!m:: Run('powershell.exe -NoProfile -ExecutionPolicy Bypass '
              . '-WindowStyle Hidden -File "org-moor-front.ps1" -Hwnd '
              . WinExist("A"), , "Hide")

  The URL is handed to emacsclientw directly: $env:EMACSCLIENT, else the
  one on PATH.  A Scoop shim on PATH is followed to the program it stands
  for, since a shim is a console program and would show a window.  Only
  when there is no emacsclientw is the org-protocol URL opened through
  Windows, which then needs the scheme registered.

.PARAMETER Hwnd
  Window to read.  Without it, the window holding the keyboard focus.

.PARAMETER Resolve
  A URL to look up instead of reading a window, for Emacs: printed are
  the URL a browser downloads it from, by the configuration's entries, and
  the Downloads folder, a line each; for an entry that gives keys, the URL
  itself, the Downloads folder and keys, for Emacs to open the URL and
  then start this script with -Press; nothing when no entry gives either.
  Nothing is opened.

.PARAMETER Press
  A URL Emacs has just opened in a browser, whose entry gives keys: once
  the window in front shows it and its title has stayed the same for a
  moment, as a page's does once it has loaded, its keys are typed there.
  Nothing is typed when it is not in front within -PressTimeout seconds.
#>
param([long]$Hwnd = 0, [switch]$WriteConfig, [string]$Resolve = '',
      [string]$Press = '', [int]$PressTimeout = 60)
$ErrorActionPreference = 'SilentlyContinue'

$ConfigHome = if ($env:XDG_CONFIG_HOME) { $env:XDG_CONFIG_HOME } else { Join-Path $env:USERPROFILE '.config' }
$ConfigFile = Join-Path (Join-Path $ConfigHome 'org-moor') 'front.json'
$ConfigTemplate = Join-Path $PSScriptRoot 'org-moor-front.json'

if ($WriteConfig) {
  $ErrorActionPreference = 'Stop'
  if (Test-Path -LiteralPath $ConfigFile) {
    Write-Output "Already there, left as it is: $ConfigFile"
  } else {
    New-Item -ItemType Directory -Force -Path (Split-Path $ConfigFile) | Out-Null
    Copy-Item -LiteralPath $ConfigTemplate -Destination $ConfigFile
    Write-Output "Written: $ConfigFile"
  }
  exit 0
}

# Sites where a page or a file is behind a sign-in, which only the browser
# has, are entries of $ConfigFile; see Read-Sites.
function Read-Sites {
  $file = if (Test-Path -LiteralPath $ConfigFile) { $ConfigFile } else { $ConfigTemplate }
  try {
    $config = Get-Content -LiteralPath $file -Raw -Encoding UTF8 | ConvertFrom-Json
    return @($config.sites)
  } catch { return @() }
}

# For a URL matching an entry's match, the browser is made to keep it: it is
# sent to the entry's download, {name} there being the group of that name in
# match, or sent the entry's keys -- SingleFile's shortcut, say -- in the
# window in front; the entry's title is what is taken off the window's title
# then.  Return the first that matches, its download filled in, or $null.
function Get-BrowserKeep($url) {
  foreach ($entry in Read-Sites) {
    if ($entry.match -and $url -match $entry.match) {
      $groups = $Matches.Clone()
      $download = if ($entry.download) {
        [regex]::Replace($entry.download, '\{(\w+)\}',
          { param($m) [string]$groups[$m.Groups[1].Value] })
      } else { '' }
      return @{ Download = $download; Keys = [string]$entry.keys; Title = [string]$entry.title
                Match = [string]$entry.match }
    }
  }
  return $null
}

# Where the browser downloads to: the Downloads folder, wherever it has
# been moved.
function Get-Downloads {
  $dir = (New-Object -ComObject Shell.Application).NameSpace('shell:Downloads').Self.Path
  if (-not $dir) { $dir = Join-Path $env:USERPROFILE 'Downloads' }
  return $dir
}

# For Emacs, taking a URL it was given rather than one in front: the URL a
# browser downloads it from, by the same entries, and where to, a line each;
# for keys, the URL itself, where to and `keys', the keys typed by -Press
# once Emacs has opened it; nothing for a URL no entry gives either for.
if ($Resolve) {
  $keep = Get-BrowserKeep $Resolve
  if ($keep -and ($keep.Download -or $keep.Keys)) {
    [Console]::OutputEncoding = New-Object Text.UTF8Encoding $false
    if ($keep.Download) {
      Write-Output $keep.Download
      Write-Output (Get-Downloads)
    } else {
      Write-Output $Resolve
      Write-Output (Get-Downloads)
      Write-Output 'keys'
    }
  }
  exit 0
}

Add-Type -AssemblyName UIAutomationClient, UIAutomationTypes
$AE = [System.Windows.Automation.AutomationElement]

# The window in front is the one holding the keyboard focus, found through
# UI Automation, which is loaded anyway: asking Windows directly would need
# C# compiled on every run, which is most of a second.
function Get-FrontWindow {
  $e = $AE::FocusedElement
  if (-not $e) { return $null }
  $walker = [System.Windows.Automation.TreeWalker]::ControlViewWalker
  while ($true) {
    $parent = $walker.GetParent($e)
    if (-not $parent -or [System.Windows.Automation.Automation]::Compare($parent, $AE::RootElement)) { break }
    $e = $parent
  }
  return $e
}

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

function Add-ZipEntry($archive, $name, $bytes) {
  $out = $archive.CreateEntry($name).Open()
  try { $out.Write($bytes, 0, $bytes.Length) } finally { $out.Dispose() }
}

# Word has no call that saves a copy and keeps working on the original --
# its SaveCopyAs fails on a document from the web, and SaveAs moves the
# open document onto the copy -- but it hands out the whole document as
# Flat OPC XML, which is the parts of the .docx, written out here as one.
function Save-WordCopy($doc, $copy) {
  [xml]$flat = $doc.WordOpenXML
  $pkg = 'http://schemas.microsoft.com/office/2006/xmlPackage'
  $ns = New-Object System.Xml.XmlNamespaceManager($flat.NameTable)
  $ns.AddNamespace('pkg', $pkg)
  $utf8 = New-Object Text.UTF8Encoding($false)
  $decl = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  $types = New-Object Text.StringBuilder
  [void]$types.Append($decl + '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">')
  Add-Type -AssemblyName System.IO.Compression
  $stream = $null; $archive = $null
  try {
    $stream = [IO.File]::Create($copy)
    $archive = New-Object System.IO.Compression.ZipArchive($stream, [System.IO.Compression.ZipArchiveMode]::Create)
    foreach ($part in $flat.SelectNodes('/pkg:package/pkg:part', $ns)) {
      $name = $part.GetAttribute('name', $pkg)
      $type = $part.GetAttribute('contentType', $pkg)
      [void]$types.Append('<Override PartName="' + [Security.SecurityElement]::Escape($name) + '" ContentType="' + [Security.SecurityElement]::Escape($type) + '"/>')
      $binary = $part.SelectSingleNode('pkg:binaryData', $ns)
      $bytes = if ($binary) { [Convert]::FromBase64String($binary.InnerText) }
               else { $utf8.GetBytes($decl + $part.SelectSingleNode('pkg:xmlData', $ns).InnerXml) }
      Add-ZipEntry $archive $name.TrimStart('/') $bytes
    }
    [void]$types.Append('</Types>')
    Add-ZipEntry $archive '[Content_Types].xml' $utf8.GetBytes($types.ToString())
  } finally {
    if ($archive) { $archive.Dispose() }
    if ($stream) { $stream.Dispose() }
  }
}

function Get-UrlFileName($url) {
  $path = ([uri]$url).AbsolutePath
  return [uri]::UnescapeDataString(($path -split '/')[-1])
}

function Get-AddressValue($e) {
  $v = $e.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)
  if (-not $v) { return '' }
  $s = $v.Current.Value
  # Firefox and Zen show the address unescaped -- spaces, Japanese -- which
  # would not survive being handed on as one argument.
  if ($s -match '^https?://') {
    try { return ([uri]$s).AbsoluteUri } catch { return '' }
  }
  if ($s -match '^file://') { return $s }
  if ($s -match '^[A-Za-z]:[\\/]') { return 'file:///' + $s }
  if ($s -match '^[\w.-]+\.[a-z]{2,}(/|$)') { return 'https://' + $s }
  return ''
}

# The address bar is the first text field in the window, ahead of the
# page; the page is searched only when it is not, since a page as large as
# PowerPoint for the web takes seconds to search.  Chromium shows it as an
# edit field, Firefox and Zen as a combo box.
function Get-BrowserUrl($root) {
  $cond = New-Object System.Windows.Automation.OrCondition(
    (New-Object System.Windows.Automation.PropertyCondition(
      $AE::ControlTypeProperty, [System.Windows.Automation.ControlType]::Edit)),
    (New-Object System.Windows.Automation.PropertyCondition(
      $AE::ControlTypeProperty, [System.Windows.Automation.ControlType]::ComboBox)))
  $first = $root.FindFirst([System.Windows.Automation.TreeScope]::Descendants, $cond)
  if ($first) {
    $url = Get-AddressValue $first
    if ($url) { return $url }
  }
  foreach ($e in $root.FindAll([System.Windows.Automation.TreeScope]::Descendants, $cond)) {
    $url = Get-AddressValue $e
    if ($url) { return $url }
  }
  return ''
}

# Zen shows only the domain in the address bar while it is not being
# typed in; the page itself, named by its title, has the whole URL.
function Get-PageUrl($root, $title) {
  $cond = New-Object System.Windows.Automation.AndCondition(
    (New-Object System.Windows.Automation.PropertyCondition(
      $AE::ControlTypeProperty, [System.Windows.Automation.ControlType]::Document)),
    (New-Object System.Windows.Automation.PropertyCondition($AE::NameProperty, $title)))
  $page = $root.FindFirst([System.Windows.Automation.TreeScope]::Descendants, $cond)
  if ($page) { return Get-AddressValue $page }
  return ''
}

function Get-FileUrlPath($url) {
  $p = [uri]::UnescapeDataString(($url -replace '^file:(//[^/]*)?', ''))
  if ($p -match '^/[A-Za-z]:') { $p = $p.Substring(1) }
  return $p -replace '/', '\'
}

$Browsers = '^(msedge|chrome|brave|firefox|zen)$'

# A browser window's title without the browser's name, and the URL of the
# page it shows.
function Get-PageTitle($window) {
  return $window.Current.Name -replace ' - (Microsoft.? Edge|Google Chrome|Brave)$', '' -replace ' - [^-]*Microsoft.? Edge$', '' -replace ' [-\u2014] (Mozilla Firefox|Zen( Browser)?)$', ''
}

function Get-WindowUrl($window, $title) {
  $url = Get-BrowserUrl $window
  if ($url -match '^https?://[^/]+/?$') {
    $page = Get-PageUrl $window $title
    if ($page) { $url = $page }
  }
  return $url
}

# For Emacs, which has just opened a URL whose entry gives keys: they are
# typed in the browser once it is in front showing a page of the entry --
# a sign-in page is not one -- and the page's title has stayed the same
# for a second and a half, as it does once the page has loaded.
if ($Press) {
  $keep = Get-BrowserKeep $Press
  if (-not ($keep -and $keep.Keys)) { exit 0 }
  $deadline = (Get-Date).AddSeconds($PressTimeout)
  $last = $null; $steady = 0
  while ((Get-Date) -lt $deadline) {
    Start-Sleep -Milliseconds 500
    $front = Get-FrontWindow
    if (-not $front) { $last = $null; continue }
    $proc = Get-Process -Id $front.Current.ProcessId
    if ($proc.ProcessName -notmatch $Browsers) { $last = $null; continue }
    $title = Get-PageTitle $front
    $url = Get-WindowUrl $front $title
    if (-not ($url -match $keep.Match)) { $last = $null; continue }
    if ($title -eq $last) { $steady++ } else { $last = $title; $steady = 0 }
    if ($steady -ge 3) {
      $wsh = New-Object -ComObject WScript.Shell
      [void]$wsh.AppActivate($proc.Id)
      Start-Sleep -Milliseconds 150
      $wsh.SendKeys($keep.Keys)
      exit 0
    }
  }
  exit 0
}

$window = if ($Hwnd -ne 0) { $AE::FromHandle([IntPtr]$Hwnd) } else { Get-FrontWindow }
if (-not $window) { exit 0 }
$handle = [IntPtr]$window.Current.NativeWindowHandle
$proc = Get-Process -Id $window.Current.ProcessId
$exe = $proc.ProcessName

$path = ''; $url = ''; $origin = ''; $arriving = ''; $since = ''
$title = $window.Current.Name

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
      # PowerPoint and Excel save a copy and keep working on the original;
      # Word is asked for its parts instead.
      if ($exe -eq 'WINWORD') {
        try { Save-WordCopy $doc $copy } catch { Remove-Item -LiteralPath $copy -Force }
      } else {
        try { $doc.SaveCopyAs($copy) } catch {}
      }
      if (Test-Path -LiteralPath $copy) { $path = $copy; $origin = $full }
      else { $url = $full }
    } else {
      $path = $full
    }
  }
  '^OUTLOOK$' {
    # A message open in its own window, else the one selected in the list,
    # saved as a .msg, which is what is taken: Outlook has no path for it.
    # Class 35 is a message's own window and 43 a mail item; 9 saves the
    # .msg as Unicode, which keeps Japanese as it is.
    $ol = [Runtime.InteropServices.Marshal]::GetActiveObject('Outlook.Application')
    if (-not $ol) { break }
    $active = $ol.ActiveWindow()
    $item = if ($active.Class -eq 35) { $active.CurrentItem }
            elseif ($active.Selection.Count -gt 0) { $active.Selection.Item(1) }
    if (-not $item -or $item.Class -ne 43) { break }
    $title = [string]$item.Subject
    $name = ($title -replace '[\\/:*?"<>|\p{Cc}]', '_').Trim(' .')
    if (-not $name) { $name = 'message' }
    if ($name.Length -gt 100) { $name = $name.Substring(0, 100) }
    $copy = Join-Path $copies ($name + '.msg')
    # An earlier copy of the same name is not to stand in for this one.
    Remove-Item -LiteralPath $copy -Force
    try { $item.SaveAs($copy, 9) } catch {}
    if (Test-Path -LiteralPath $copy) { $path = $copy }
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
  $Browsers {
    $title = Get-PageTitle $window
    $url = Get-WindowUrl $window $title
    $title = $title -replace '\.(pptx?|docx?|xlsx?|pdf)$', ''
    if ($url -match '^file:') {
      $path = Get-FileUrlPath $url; $url = ''; $title = ''
    } elseif ($url) {
      $keep = Get-BrowserKeep $url
      if ($keep -and $keep.Title) { $title = $title -replace $keep.Title, '' }
      if ($keep -and ($keep.Download -or $keep.Keys)) {
        $arriving = Get-Downloads
        $since = [string][DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
        if ($keep.Download) {
          Start-Process -FilePath $proc.Path -ArgumentList $keep.Download
        } else {
          $wsh = New-Object -ComObject WScript.Shell
          [void]$wsh.AppActivate($proc.Id)
          Start-Sleep -Milliseconds 150
          $wsh.SendKeys($keep.Keys)
        }
      }
    }
  }
}

if (-not $path -and -not $url) { exit 0 }

$q = if ($path) { 'path=' + [uri]::EscapeDataString($path) } else { 'url=' + [uri]::EscapeDataString($url) }
if ($origin) { $q += '&origin=' + [uri]::EscapeDataString($origin) }
if ($arriving) { $q += '&arriving=' + [uri]::EscapeDataString($arriving) + '&since=' + $since }
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
