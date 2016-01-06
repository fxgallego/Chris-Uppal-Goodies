| package |
package := Package name: 'CU Safer Maths'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

This adds a few methods to LargeInteger and Fraction which eliminate some avoidable floating point overflows when working with very high precision Integers and Fractions.

Note that *most* such overflows are *not* avoidable!!
'.

package basicPackageVersion: '1.00'.


package methodNames
	add: #Fraction -> #ln;
	add: #Fraction -> #log;
	add: #Fraction -> #sqrt;
	add: #LargeInteger -> #ln;
	add: #LargeInteger -> #log;
	add: #LargeInteger -> #safeLog2;
	add: #LargeInteger -> #sqrt;
	add: #Number -> #isqrt;
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

!Fraction methodsFor!

ln
	"Answer a <Float> which is the natural logarithm of the receiver."
#CUadded.

	"overridden to avoid overflowing intermediate Floats"
	^ numerator ln - denominator ln.
!

log
	"Answer the log to the base 10 of the receiver"
#CUadded.

	"overridden to avoid overflowing intermediate Floats"
	^ numerator log - denominator log.
!

sqrt
	"Answer a <number> which is the positive square root of the receiver."
#CUadded.

	"overridden to avoid overflowing intermediate Floats"
	^ numerator sqrt / denominator sqrt.! !
!Fraction categoriesFor: #ln!mathematical!public! !
!Fraction categoriesFor: #log!mathematical!public! !
!Fraction categoriesFor: #sqrt!mathematical!public! !

!LargeInteger methodsFor!

ln
	"Answer a <Float> which is the natural logarithm of the receiver."
#CUadded.

	"overridden to correct for overflowing intermediate Floats"
	[^ super ln]
		on: FloatingPointException
		do: [:error | error isOverflow
				ifTrue: [^ ##(2 ln) * self safeLog2]
				ifFalse: [error pass]].!

log
	"Answer the log to the base 10 of the receiver"
#CUadded.

	"overridden to correct for overflowing intermediate Floats"
	[^ super log]
		on: FloatingPointException
		do: [:error | error isOverflow
				ifTrue: [^ ##(2 log) * self safeLog2]
				ifFalse: [error pass]].
!

safeLog2
	"private - Answer a <Float> which is the log to the base 2 of the receiver."
	"This uses an algorithm which avoids floating point overflow by bit-twiddling
	with the underlying representation.  Note that we assume that we
	are already known to be > 0"
	| source digits ignore approx scale |
#CUadded.

	source := self normalize.
	digits := source digitSize.

	"we use at most 64bits of precision and will scale by the number of digits we ignored"
	ignore := digits - 8 max: 1.
	scale := ignore - 1 * 8.

	"construct approximation"
	approx := 0.0.
	digits to: ignore by: -1 do: [ :i | approx := approx * 256.0 + (source byteAt: i) asFloat].

	"now we can do the scaling by *addition* after taking the log"
	^ (approx log: 2) + scale.
!

sqrt
	"Answer a <number> which is the positive square root of the receiver."
#CUadded.

	"overridden to correct for overflowing intermediate Floats"

	[^ super sqrt]
		on: FloatingPointException
		do: [:error | error isOverflow ifFalse: [error pass]].

	"didn't work, do it the hard way"
	^ 2 raisedTo: (self safeLog2 / 2).! !
!LargeInteger categoriesFor: #ln!mathematical!public! !
!LargeInteger categoriesFor: #log!mathematical!public! !
!LargeInteger categoriesFor: #safeLog2!mathematical!private! !
!LargeInteger categoriesFor: #sqrt!mathematical!public! !

!Number methodsFor!

isqrt
	"Answer the <integer> square root of the receiver"
	| digits result |
#CUadded.

	"this amounts to a binary chop across the space of all positive Integers with
	at most half as many bits as the receiver"

	self < 0 ifTrue: [ArithmeticError signal: 'Square root of negative number'].

	"how many binary digits can root have ?"
	digits := 0.
	[(1 bitShift: digits) < self] whileTrue: 	[digits := digits + 1].
	digits := digits // 2.

	"try setting each bit of the root in turn, starting at the high bit, if doing so doesn't
	make the root too high, then it should be set in the final result"
	result := 0.
	digits to: 0 by: -1 do:
		[:place || try |
		try := result bitOr: (1 bitShift: place).
		try * try > self ifFalse: [result := try]].

	^ result.! !
!Number categoriesFor: #isqrt!mathematical!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

