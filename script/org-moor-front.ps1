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

  A file opened in a browser is taken as a file: one on disk by its path,
  and one behind a sign-in -- PowerPoint or Word for the web, a PDF in
  SharePoint's viewer, a Confluence page as a PDF; see $BrowserDownloads --
  by having the browser, which is signed in, download it.  Emacs is not kept waiting for that: it is told the download is
  arriving in the Downloads folder, and takes it from there once whole,
  with its URL kept as where it came from.  Any other URL Emacs fetches
  itself.

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
#>
param([long]$Hwnd = 0)
$ErrorActionPreference = 'SilentlyContinue'

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

$window = if ($Hwnd -ne 0) { $AE::FromHandle([IntPtr]$Hwnd) } else { Get-FrontWindow }
if (-not $window) { exit 0 }
$handle = [IntPtr]$window.Current.NativeWindowHandle
$proc = Get-Process -Id $window.Current.ProcessId
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

function Get-AddressValue($e) {
  $v = $e.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)
  if (-not $v) { return '' }
  $s = $v.Current.Value
  # Firefox and Zen show the address unescaped -- spaces, Japanese -- which
  # would not survive being handed on as one argument.
  if ($s -match '^https?://') { return ([uri]$s).AbsoluteUri }
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

# Sites where a page or a file is behind a sign-in, which only the browser
# has: for a URL matching Match, the browser downloads Download, and Emacs
# takes what arrives.  {name} in Download is the group of that name in
# Match.  A row is all a new site needs.
$BrowserDownloads = @(
  # PowerPoint, Word or Excel for the web, showing a file by its id.
  @{ Match = '^https://(?<host>[^/]+\.sharepoint\.com)(/:[a-z]:/[a-z])?(?<site>/(sites|teams|personal)/[^/?]+)?/.*[?&]sourcedoc=(%7B|\{)?(?<id>[0-9a-fA-F-]{36})'
     Download = 'https://{host}{site}/_layouts/15/download.aspx?UniqueId={id}' },
  # A library's viewer -- a PDF, say -- showing a file by its path.
  @{ Match = '^https://(?<host>[^/]+\.sharepoint\.com)(/:[a-z]:/[a-z])?(?<site>/(sites|teams|personal)/[^/?]+)?/.*[?&]id=(?<path>[^&#]+(\.|%2[Ee])\w+)(&|#|$)'
     Download = 'https://{host}{site}/_layouts/15/download.aspx?SourceUrl={path}' },
  # A Confluence Cloud page, as a PDF.
  @{ Match = '^https://(?<host>[^/]+\.atlassian\.net)/wiki/(spaces/[^/]+/pages/(edit-v2/)?|.*[?&]pageId=)(?<id>\d+)'
     Download = 'https://{host}/wiki/spaces/flyingpdf/pdfpageexport.action?pageId={id}' }
)

function Get-BrowserDownload($url) {
  foreach ($row in $BrowserDownloads) {
    if ($url -match $row.Match) {
      $groups = $Matches.Clone()
      return [regex]::Replace($row.Download, '\{(\w+)\}',
        { param($m) [string]$groups[$m.Groups[1].Value] })
    }
  }
  return ''
}

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
    $url = Get-BrowserUrl $window
    $title = $title -replace ' - (Microsoft.? Edge|Google Chrome|Brave)$', '' -replace ' - [^-]*Microsoft.? Edge$', '' -replace ' [-\u2014] (Mozilla Firefox|Zen( Browser)?)$', ''
    if ($url -match '^https?://[^/]+/?$') {
      $page = Get-PageUrl $window $title
      if ($page) { $url = $page }
    }
    $title = $title -replace '\.(pptx?|docx?|xlsx?|pdf)$', '' -replace ' - Confluence$', ''
    if ($url -match '^file:') {
      $path = Get-FileUrlPath $url; $url = ''; $title = ''
    } elseif ($url) {
      $download = Get-BrowserDownload $url
      if ($download) {
        $arriving = (New-Object -ComObject Shell.Application).NameSpace('shell:Downloads').Self.Path
        if (-not $arriving) { $arriving = Join-Path $env:USERPROFILE 'Downloads' }
        $since = [string][DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
        Start-Process -FilePath $proc.Path -ArgumentList $download
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
