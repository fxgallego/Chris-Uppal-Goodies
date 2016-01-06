| package |
package := Package name: 'CU Binary Arrays'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A few more binary arrays to complete the portfolio provided by Dolphin.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

2.00
-	Changed from #at:[put:] to #uncheckedAt:[put:] following D5.1 pl2.

1.00
-	First release.

'.

package basicPackageVersion: '2.00'.


package classNames
	add: #BYTEArray;
	add: #SBYTEArray;
	add: #SQWORD;
	add: #SQWORDArray;
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

ExternalArray subclass: #BYTEArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalArray subclass: #SQWORDArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BYTEArray subclass: #SBYTEArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalInteger subclass: #SQWORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

BYTEArray guid: (GUID fromString: '{E9864A7F-836E-494C-95E2-1C90C3606712}')!
BYTEArray comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

The superclass comment states that there is no need for this class since ByteArray performs the same service, but ByteArray>>fromAddress:length: takes a *copy*.'!
!BYTEArray categoriesForClass!Unclassified! !
!BYTEArray methodsFor!

elementClass
	"Answer the class of <ExternalStructure> used to represent elements of the receiver."

	^ BYTE.!

uncheckedAt: index
	"Private - Answer the 8-bit unsigned integer value at the specified index in the receiver."

	^ bytes byteAtOffset: index - 1.
!

uncheckedAt: index put: value
	"Private - Replace the 8-bit unsigned integer value at the specified index in the receiver
	with the Integer argument, value."

	^ bytes byteAtOffset: index - 1 put: value.! !
!BYTEArray categoriesFor: #elementClass!constants!public! !
!BYTEArray categoriesFor: #uncheckedAt:!accessing!private! !
!BYTEArray categoriesFor: #uncheckedAt:put:!accessing!private! !

!BYTEArray class methodsFor!

elementSize
	"Private - Answer the size of the receiver's constituent elements."
	
	^ 1.! !
!BYTEArray class categoriesFor: #elementSize!constants!private! !

SQWORDArray guid: (GUID fromString: '{07075AFB-0307-4123-96FB-2FF3F27E28EC}')!
SQWORDArray comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org'!
!SQWORDArray categoriesForClass!Unclassified! !
!SQWORDArray methodsFor!

elementClass
	"Answer the class of <ExternalStructure> used to represent elements of the receiver."

	^ SQWORD.!

uncheckedAt: index
	"Private - Answer the 8-byte signed integer value at the specified index in the receiver."

	^ bytes sqwordAtOffset: (index - 1) * 8.!

uncheckedAt: index put: value
	"Private - Replace the 8-byte signed integer value at the specified index in the receiver
	with the Integer argument, value."

	^ bytes sqwordAtOffset: (index - 1) * 8 put: value.! !
!SQWORDArray categoriesFor: #elementClass!constants!public! !
!SQWORDArray categoriesFor: #uncheckedAt:!accessing!private! !
!SQWORDArray categoriesFor: #uncheckedAt:put:!accessing!private! !

!SQWORDArray class methodsFor!

elementSize
	"Private - Answer the size of the receiver's constituent elements."
	
	^ 8.! !
!SQWORDArray class categoriesFor: #elementSize!constants!private! !

SBYTEArray guid: (GUID fromString: '{4F40D7B0-A9C7-42FB-9BB2-4EB4B1B64AF1}')!
SBYTEArray comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!SBYTEArray categoriesForClass!Unclassified! !
!SBYTEArray methodsFor!

elementClass
	"Answer the class of <ExternalStructure> used to represent elements of the receiver."

	^ SBYTE.!

uncheckedAt: index
	"Private - Answer the 8-bit signed integer value at the specified index in the receiver."

	^ bytes sbyteAtOffset: index - 1.
!

uncheckedAt: index put: value
	"Private - Replace the 8-bit signed integer value at the specified index in the receiver
	with the Integer argument, value."

	^ bytes sbyteAtOffset: index - 1 put: value.! !
!SBYTEArray categoriesFor: #elementClass!constants!public! !
!SBYTEArray categoriesFor: #uncheckedAt:!accessing!private! !
!SBYTEArray categoriesFor: #uncheckedAt:put:!accessing!private! !

SQWORD guid: (GUID fromString: '{C385404D-F1AC-4738-99BF-22CC7C53D5E4}')!
SQWORD comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!SQWORD categoriesForClass!Unclassified! !
!SQWORD methodsFor!

value
	"Answer the receiver's value field as a Smalltalk object."

	^ bytes sqwordAtOffset: 0.!

value: anObject
	"Set the receiver's value field to the value of anObject."

	bytes sqwordAtOffset: 0 put: anObject.! !
!SQWORD categoriesFor: #value!**compiled accessors**!public! !
!SQWORD categoriesFor: #value:!**compiled accessors**!public! !

!SQWORD class methodsFor!

defineFields
	"Define the fields of the SQWORD 'structure'. ExternalInteger subclasses
	have a single value.
		SQWORD recompileDefinition
	"

	self defineField: #value type: SQWORDField new! !
!SQWORD class categoriesFor: #defineFields!initializing!public! !

"Binary Globals"!

"Resources"!

