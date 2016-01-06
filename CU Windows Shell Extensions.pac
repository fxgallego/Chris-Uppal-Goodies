| package |
package := Package name: 'CU Windows Shell Extensions'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Trivial extensions to the Windows Shell package.

E.g; Inspect:
Dictionary withAll: (
	Win32Constants associations select: [:each | each key beginsWith: ''CSIDL_'']
	thenCollect: [:each | each key -> (ShellLibrary default getSpecialFolderPath: each value)]).

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.

package basicScriptAt: #preinstall put: 'Smalltalk at: #Win32Constants ifPresent: [:it |
it
	at: ''CSIDL_DESKTOP''			put: 16r0000;
	at: ''CSIDL_INTERNET''			put: 16r0001;
	at: ''CSIDL_PROGRAMS''			put: 16r0002;
	at: ''CSIDL_CONTROLS''			put: 16r0003;
	at: ''CSIDL_PRINTERS''			put: 16r0004;
	at: ''CSIDL_PERSONAL''			put: 16r0005;
	at: ''CSIDL_FAVORITES''			put: 16r0006;
	at: ''CSIDL_STARTUP''			put: 16r0007;
	at: ''CSIDL_RECENT''			put: 16r0008;
	at: ''CSIDL_SENDTO''			put: 16r0009;
	at: ''CSIDL_BITBUCKET''			put: 16r000A;
	at: ''CSIDL_STARTMENU''			put: 16r000B;
	at: ''CSIDL_DESKTOPDIRECTORY''	put: 16r0010;
	at: ''CSIDL_DRIVES''				put: 16r0011;
	at: ''CSIDL_NETWORK''			put: 16r0012;
	at: ''CSIDL_NETHOOD''			put: 16r0013;
	at: ''CSIDL_FONTS''				put: 16r0014;
	at: ''CSIDL_TEMPLATES''			put: 16r0015;
	at: ''CSIDL_COMMON_STARTMENU''	put: 16r0016;
	at: ''CSIDL_COMMON_PROGRAMS''	put: 16r0017;
	at: ''CSIDL_COMMON_STARTUP''		put: 16r0018;
	at: ''CSIDL_COMMON_DESKTOPDIRECTORY''	put: 16r0019;
	at: ''CSIDL_APPDATA''			put: 16r001A;
	at: ''CSIDL_PRINTHOOD''			put: 16r001B;
	at: ''CSIDL_ALTSTARTUP''			put: 16r001D;	"DBCS"
	at: ''CSIDL_COMMON_ALTSTARTUP''	put: 16r001E;	"DBCS"
	at: ''CSIDL_COMMON_FAVORITES''	put: 16r001F;
	at: ''CSIDL_INTERNET_CACHE''		put: 16r0020;
	at: ''CSIDL_COOKIES''			put: 16r0021;
	at: ''CSIDL_HISTORY''			put: 16r0022].
!!'.

package methodNames
	add: #BrowseFolderDialog -> #allowNetworkFolders;
	add: #ShellLibrary -> #getSpecialFolderPath:;
	add: #ShellLibrary -> #getSpecialFolderPath:createIfAbsent:;
	add: #ShellLibrary -> #SHGetSpecialFolderPathHWND:lpszPath:nFolder:fCreate:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!BrowseFolderDialog methodsFor!

allowNetworkFolders
#CUadded.

	style := style bitXor: BIF_DONTGOBELOWDOMAIN.! !
!BrowseFolderDialog categoriesFor: #allowNetworkFolders!accessing!public! !

!ShellLibrary methodsFor!

getSpecialFolderPath: anInteger
	"Answer the <readableString> location of the special folder identified by
	the CSIDL_* anInteger.
	If the folder is not a filesystem directory, then answer nil.

	E.g:
		ShellLibrary default getSpecialFolderPath: CSIDL_FAVORITES.

	or (should answer nil):
		ShellLibrary default getSpecialFolderPath: CSIDL_NETWORK.
	"

#CUadded.

	^ self getSpecialFolderPath: anInteger createIfAbsent: false.!

getSpecialFolderPath: anInteger createIfAbsent: aBool
	"Answer the <readableString> location of the special folder identified by
	the CSIDL_* anInteger; optionally creating the folder if it does not already exist.
	If the folder is not a filesystem directory, then answer nil."

	| pathname |
#CUadded.

	pathname := File pathBuffer.
	^ (self SHGetSpecialFolderPathHWND: nil lpszPath: pathname nFolder: anInteger fCreate: aBool)
		ifTrue: [pathname trimNulls]
		ifFalse: [nil].
!

SHGetSpecialFolderPathHWND: hwndOwner lpszPath: lpszPath nFolder: nFolder fCreate: fCreate
	"
	BOOL SHGetSpecialFolderPathA(HWND hwndOwner, LPSTR lpszPath, int nFolder, BOOL fCreate)
	"

	<stdcall: bool SHGetSpecialFolderPathA handle lpstr sdword bool>
#CUadded.
	^self invalidCall! !
!ShellLibrary categoriesFor: #getSpecialFolderPath:!accessing!public! !
!ShellLibrary categoriesFor: #getSpecialFolderPath:createIfAbsent:!accessing!public! !
!ShellLibrary categoriesFor: #SHGetSpecialFolderPathHWND:lpszPath:nFolder:fCreate:!public!win32 functions-shell library! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

