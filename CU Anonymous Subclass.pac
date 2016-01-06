| package |
package := Package name: 'CU Anonymous Subclass'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Small framework for making anonymous subclasses (i.e. not linked into the browsable hierarchy) of existing classes.

(P.S. yes it does really only consist of one loose method!!)

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package methodNames
	add: #Class -> #anonymousSubclass:;
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

!Class methodsFor!

anonymousSubclass: aString
	"Answer a new subclass of the receiver which is not installed as a global variable, nor
	linked into the class hierarchy proper.  It will consider its name to be aString, but is -- of
	course -- not accessible under that name"

	| newMeta newClass |
#CUadded.

	newMeta := (Metaclass new)
		superclass: self class;
		methodDictionary: MethodDictionary new;
		instanceSpec: self class instanceSpec;
		yourself.

	newClass := (newMeta new)
		superclass: self;
		methodDictionary: MethodDictionary new;
		instanceSpec: self instanceSpec;
		setName: aString asSymbol;			"normal classes have Symbol names, so we might as well too"
		yourself.

	^ newClass.
! !
!Class categoriesFor: #anonymousSubclass:!anonymous subclassing!object methods!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

