| package |
package := Package name: 'CU Storage Size'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

A single method that embeds the knowledge about how much space an object really takes up in memory.  (Based on posts by Blair McGashan over the years.)

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.


package methodNames
	add: #Object -> #storageSize;
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

!Object methodsFor!

storageSize
	"Answer the number of bytes of VM memory used to hold the
	receiver.
	E.g:
		Object allSubinstances inject: 0 into: [:a :e | a + e storageSize].
	"

	| size |
#CUadded.

	self isImmediate ifTrue: [^ 0].
	size := self basicSize + self class instSize.
	self class isPointers
		ifTrue: [size := size * 4]
		ifFalse: [self class isNullTerminated ifTrue: [size := size + 1]].
	size := (size + 3) bitAnd: -4.	"round up to multiple of 4 bytes"
	^ size + 16.! !
!Object categoriesFor: #storageSize!memory calculations!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

