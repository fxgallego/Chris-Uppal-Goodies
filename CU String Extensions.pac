| package |
package := Package name: 'CU String Extensions'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

A few extra methods for String.  Specifically versions of #sprintf that take more arguments and access to C strcmp() (which provide case-*sensitive* ordering of Strings).

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.


package methodNames
	add: #CRTLibrary -> #_snprintf:count:format:with:with:with:;
	add: #CRTLibrary -> #_snprintf:count:format:with:with:with:with:;
	add: #CRTLibrary -> #_snprintf:count:format:with:with:with:with:with:;
	add: #CRTLibrary -> #_snprintf:count:format:with:with:with:with:with:with:;
	add: #CRTLibrary -> #strcmp:string2:;
	add: #String -> #sprintfWith:with:with:;
	add: #String -> #sprintfWith:with:with:with:;
	add: #String -> #sprintfWith:with:with:with:with:;
	add: #String -> #sprintfWith:with:with:with:with:with:;
	add: #String -> #strcmp:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!CRTLibrary methodsFor!

_snprintf: buffer count: maxbuf format: format with: arg1 with: arg2 with: arg3
	"Private - Write data formatted by the format string into the buffer.
	see _snprintf:count:format:with:with:with: for further information."

	<cdecl: sdword _snprintf lpvoid sdword lpstr lpvoid lpvoid lpvoid>
#CUadded.
	^self invalidCall!

_snprintf: buffer count: maxbuf format: format with: arg1 with: arg2 with: arg3 with: arg4
	"Private - Write data formatted by the format string into the buffer.
	see _snprintf:count:format:with:with:with: for further information."

	<cdecl: sdword _snprintf lpvoid sdword lpstr lpvoid lpvoid lpvoid lpvoid>
#CUadded.
	^self invalidCall!

_snprintf: buffer count: maxbuf format: format with: arg1 with: arg2 with: arg3 with: arg4 with: arg5
	"Private - Write data formatted by the format string into the buffer.
	see _snprintf:count:format:with:with:with: for further information."

	<cdecl: sdword _snprintf lpvoid sdword lpstr lpvoid lpvoid lpvoid lpvoid lpvoid>
#CUadded.
	^self invalidCall!

_snprintf: buffer count: maxbuf format: format with: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	"Private - Write data formatted by the format string into the buffer.
	see _snprintf:count:format:with:with:with: for further information."

	<cdecl: sdword _snprintf lpvoid sdword lpstr lpvoid lpvoid lpvoid lpvoid lpvoid lpvoid>
#CUadded.
	^self invalidCall!

strcmp: string1 string2: string2
	"Answer the order between the <String> arguments, string1 and string2.

		int strcmp( const char *string1, const char *string2 );
	"

	<cdecl: sdword strcmp lpstr lpstr>
#CUadded.
	^self invalidCall.! !
!CRTLibrary categoriesFor: #_snprintf:count:format:with:with:with:!CRT functions-stream I/O!CU-utils!private! !
!CRTLibrary categoriesFor: #_snprintf:count:format:with:with:with:with:!CRT functions-stream I/O!CU-utils!private! !
!CRTLibrary categoriesFor: #_snprintf:count:format:with:with:with:with:with:!CRT functions-stream I/O!CU-utils!private! !
!CRTLibrary categoriesFor: #_snprintf:count:format:with:with:with:with:with:with:!CRT functions-stream I/O!CU-utils!private! !
!CRTLibrary categoriesFor: #strcmp:string2:!CRT functions-string manipulation!CU-utils!public! !

!String methodsFor!

sprintfWith: arg1 with: arg2 with: arg3
	"Answer a String which is a message formatted from the receiver (assumed to be a C-printf
	format String) with substituations from the remaining argument(s).
	Note: This is much faster than formatWith:with:with:."

	| n crt buf size |
#CUadded.
	crt := CRTLibrary default.
	size := self size + 192.
	[
		buf := String new: size.
		n := crt _snprintf: buf count: size format: self with: arg1 with: arg2 with: arg3.
		n < 0] whileTrue: [size := size * 2].
	^buf copyFrom: 1 to: n!

sprintfWith: arg1 with: arg2 with: arg3 with: arg4
	"Answer a String which is a message formatted from the receiver (assumed to be a C-printf
	format String) with substituations from the remaining argument(s).
	Note: This is much faster than formatWith:with:with:."

	| n crt buf size |
#CUadded.
	crt := CRTLibrary default.
	size := self size + 192.
	[
		buf := String new: size.
		n := crt _snprintf: buf count: size format: self with: arg1 with: arg2 with: arg3 with: arg4.
		n < 0] whileTrue: [size := size * 2].
	^buf copyFrom: 1 to: n!

sprintfWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5
	"Answer a String which is a message formatted from the receiver (assumed to be a C-printf
	format String) with substituations from the remaining argument(s).
	Note: This is much faster than formatWith:with:with:."

	| n crt buf size |
#CUadded.
	crt := CRTLibrary default.
	size := self size + 192.
	[
		buf := String new: size.
		n := crt _snprintf: buf count: size format: self with: arg1 with: arg2 with: arg3 with: arg4 with: arg5.
		n < 0] whileTrue: [size := size * 2].
	^buf copyFrom: 1 to: n!

sprintfWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	"Answer a String which is a message formatted from the receiver (assumed to be a C-printf
	format String) with substituations from the remaining argument(s).
	Note: This is much faster than formatWith:with:with:."

	| n crt buf size |
#CUadded.
	crt := CRTLibrary default.
	size := self size + 192.
	[
		buf := String new: size.
		n := crt _snprintf: buf count: size format: self with: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6.
		n < 0] whileTrue: [size := size * 2].
	^buf copyFrom: 1 to: n!

strcmp: comparand
	"Direct access to C runtime strcmp()."
#CUadded.

	^ CRTLibrary default strcmp: self string2: comparand.! !
!String categoriesFor: #sprintfWith:with:with:!CU-utils!printing!public! !
!String categoriesFor: #sprintfWith:with:with:with:!CU-utils!printing!public! !
!String categoriesFor: #sprintfWith:with:with:with:with:!CU-utils!printing!public! !
!String categoriesFor: #sprintfWith:with:with:with:with:with:!CU-utils!printing!public! !
!String categoriesFor: #strcmp:!comparing!CU-utils!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

