| package |
package := Package name: 'CU Java Quasi-UTF8'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Contains routines for using the wierd quasi-UTF8 format used by Java JVMs and hence by JNI and Java classfiles.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package methodNames
	add: #ByteArray -> #asJavaQuasiUTF8EncodedByteArray;
	add: #String -> #asJavaQuasiUTF8EncodedByteArray;
	add: #String -> #asJavaQuasiUTF8EncodedString;
	add: #String -> #needsJavaQuasiUTF8Encoding;
	add: 'String class' -> #fromJavaQuasiUTF8EncodedByteArray:;
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

!ByteArray methodsFor!

asJavaQuasiUTF8EncodedByteArray
	"Answer the receiver as a ByteArray encoded in the wierd variant of UTF8 that
	is used in JNI and the JVM classfile format.
	Since the receiver is already a ByteArray, it is assumed to be properly encoded
	already"
#CUadded.

	^ self.! !
!ByteArray categoriesFor: #asJavaQuasiUTF8EncodedByteArray!converting!public! !

!String methodsFor!

asJavaQuasiUTF8EncodedByteArray
	"Answer the receiver as a ByteArray encoded in the wierd variant of UTF8 that
	is used in JNI and the JVM classfile format."

	| bytes |

#CUadded.

	"Note: we assume that we are not already utf8 encoded, i.e. that we contains characters
	in just the range [0..255].  So we have to encode null and and characters with the top bit
	set as two-bytes each, but we never need to worry about 3 (or more) byte encodings since
	Dolphin Strings can't contain characters > 255"

	"quick scan to see if we're OK already"
	self needsJavaQuasiUTF8Encoding ifFalse: [^ self asByteArray].

	bytes := ByteArray writeStream: self size.

	self do:
		[:each || code |
		code := each codePoint.
		(code = 0 or: [code anyMask: 16r80])
			ifTrue:
				[| x y |
				x := ((code bitShift: -6) bitAnd: 16r1F) bitOr: 16rC0.
				y := (code bitAnd: 16r3F) bitOr: 16r80.
			"	self assert: [code = (((x bitAnd: 16r1F) bitShift: 6) bitOr: (y bitAnd: 16r3F))].	"
				bytes nextPut: x; nextPut: y]
			ifFalse:
				[bytes nextPut: code]].

	^ bytes contents.

"little test:
	input := (0 to: 255) asByteArray asString.
	encoded := input asJavaQuasiUTF8EncodedByteArray.
	output := self fromJavaQuasiUTF8EncodedByteArray: encoded.
	input = output. 
"!

asJavaQuasiUTF8EncodedString
	"Answer the receiver encoded in the wierd variant of UTF8 that
	is used in JNI and the JVM classfile format."

	| answer |

#CUadded.

	"Note: we assume that we are not already utf8 encoded, i.e. that we contains characters
	in just the range [0..255].  So we have to encode null and and characters with the top bit
	set as two-bytes each, but we never need to worry about 3 (or more) byte encodings since
	Dolphin Strings can't contain characters > 255"

	"quick scan to see if we're OK already"
	self needsJavaQuasiUTF8Encoding ifFalse: [^ self].

	answer := self class writeStream: self size.

	self do:
		[:each || code |
		code := each codePoint.
		(code = 0 or: [code anyMask: 16r80])
			ifTrue:
				[| x y |
				x := ((code bitShift: -6) bitAnd: 16r1F) bitOr: 16rC0.
				y := (code bitAnd: 16r3F) bitOr: 16r80.
			"	self assert: [code = (((x bitAnd: 16r1F) bitShift: 6) bitOr: (y bitAnd: 16r3F))].	"
				answer
					nextPut: (Character codePoint: x);
					nextPut: (Character codePoint: y)]
			ifFalse:
				[answer nextPut: each]].

	^ answer contents.

"little test:
	input := (0 to: 255) asByteArray asString.
	encoded := input asJavaQuasiUTF8EncodedString.
	output := self fromJavaQuasiUTF8EncodedString: encoded.
	input = output. 
"!

needsJavaQuasiUTF8Encoding
	"Answer true iff the receiver ever needs to be encoded before being passed to JNI or used
	as a 'UTF8' string in a Java classfile.  This is the case if we contain a null or any char
	with it's high bit set"

#CUadded.

	"we want this to be as fast as possible, so we take a shortcut and use #basicAt: rather
	than using #at and sending #codePoint (or #asInteger) to the elements, the obvious loop:
		^ self anySatisfy: [:each || code | code := each codePoint. code = 0 or: [code anyMask: 16r80]].
	takes 23 usecs to test 'java/lang/Class' on my machine, as opposed to 5 usecs for the loop below"
	1 to: self size do:
		[:i || code |
		code := self basicAt: i.
		(code = 0 or: [code anyMask: 16r80]) ifTrue: [^ true]].

	^ false.
! !
!String categoriesFor: #asJavaQuasiUTF8EncodedByteArray!converting!public! !
!String categoriesFor: #asJavaQuasiUTF8EncodedString!converting!public! !
!String categoriesFor: #needsJavaQuasiUTF8Encoding!converting!public!testing! !

!String class methodsFor!

fromJavaQuasiUTF8EncodedByteArray: aByteArray
	"Answer a new instance of the receiver initialised by decoding the nearly-but-not-quite UTF8
	encoded byte array."

	| in out |

#CUadded.

	"quick check to see if we can do it faster"
	(aByteArray allSatisfy: [:each | each < 128]) ifTrue: [^ aByteArray asString].

	in := aByteArray readStream.
	out := self writeStream: aByteArray size.

	[in atEnd] whileFalse:
		[| code |
		code := in next.
		(code anyMask: 16r80) ifTrue:
			[| x y |
			x := code.
			y := in next.
			(code allMask: 16rE0)
				ifTrue:
					[| z |
					z := in next.
					code := (((x bitAnd: 16rF) bitShift: 12) bitOr: ((y bitAnd: 16r3F) bitShift: 6)) bitOr: (z bitAnd: 16r3F)]
				ifFalse:
					[code := ((x bitAnd: 16r1F) bitShift: 6) bitOr: (y bitAnd: 16r3F)]].

		"this will throw an exception if code >= 256"
		out nextPut: (Character codePoint: code)].

	^ out contents.

"little test:
	input := (0 to: 255) asByteArray asString.
	encoded := input asJavaQuasiUTF8EncodedByteArray.
	output := self fromJavaQuasiUTF8EncodedByteArray: encoded.
	input = output. 
"! !
!String class categoriesFor: #fromJavaQuasiUTF8EncodedByteArray:!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

