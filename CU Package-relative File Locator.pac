| package |
package := Package name: 'CU Package-relative File Locator'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002-2005.
chris.uppal@metagnostic.org

PackageRelativeFileLocator is a file locator that works relative to the folder containing a *named* package.  If the file locator is used in a deployed app, then it falls back to working image-relative.

PackageResourceLocator is similar except that it knows to look for resources in a specific sub-folder of the package folder or image folder.  This has advantages in brevity, but is mostly valuable for looking up resources that may be bound into a deployed executable.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

3.00
-	Added PackageResourceLocator.

2.00
-	Added  a sensible #printOn: method.

1.00
-	First release.'.

package basicPackageVersion: '3.01'.


package classNames
	add: #PackageRelativeFileLocator;
	add: #PackageResourceLocator;
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

RelativeFileLocator subclass: #PackageRelativeFileLocator
	instanceVariableNames: 'packageName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RelativeFileLocator subclass: #PackageResourceLocator
	instanceVariableNames: 'packageName subFolder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

PackageRelativeFileLocator guid: (GUID fromString: '{01881AC6-D4EC-4FC2-BE62-F07236BE9753}')!
PackageRelativeFileLocator comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

PackageRelativeFileLocator is a file locator that works relative to the folder containing a *named* package.  If the file locator is used in a deployed app, then it falls back to working image-relative.  The nett effect is that you can put icons, etc, in the same folder as the package they belong to, rather than messing around copying them into strange places.

	-- chris'!
!PackageRelativeFileLocator categoriesForClass!Unclassified! !
!PackageRelativeFileLocator methodsFor!

basePath
	"since this stuff isn't bloody documented, I can only guess at what this method
	is *supposed* to do.  What it actually *does* is answer the directory where
	our package lives"

	^ self package
		ifNil: [super basePath]
		ifNotNil: [:it | File splitPathFrom: it packageFileName].!

hack
	self become: (PackageResourceLocator packageNamed: packageName).!

package
	"answer the Package relative to the location of which we will handle file paths or nil if the package
	doesn't exist or Package has been stripped out of the image"

	^ Smalltalk at: #Package ifPresent: [:it | it manager packageNamed: packageName ifNone: [nil]].!

packageName
	"answer the String name of the Package relative to the location of which we will handle file paths"

	^ packageName.!

packageName: aString
	"private -- set the String name of the Package relative to the location of which we will handle file paths"

	packageName := aString.!

printOn: aStream
	"append a developer friendly representation of ourself to the given stream.
	This implementation allows the reciever to be recreated (e.g. in an Inspector)"

	aStream
		display: self class;
		display: ' packageNamed: ';
		print: packageName.
! !
!PackageRelativeFileLocator categoriesFor: #basePath!accessing!public! !
!PackageRelativeFileLocator categoriesFor: #hack!printing!public! !
!PackageRelativeFileLocator categoriesFor: #package!accessing!public! !
!PackageRelativeFileLocator categoriesFor: #packageName!accessing!public! !
!PackageRelativeFileLocator categoriesFor: #packageName:!initializing!private! !
!PackageRelativeFileLocator categoriesFor: #printOn:!printing!public! !

!PackageRelativeFileLocator class methodsFor!

package: aPackage
	"answer an instance that is configured to interpret files relative to
	the folder containing aPackage.  It is an error if the package has
	not yet been saved.  Note that it doesn't hold onto a reference to
	the package, just the name, and if (at runtime) the package is not
	found then it'll default to image-relative handling"

	^ self packageNamed: aPackage name.
!

packageNamed: aString
	"answer an instance that is configured to interpret files relative to
	the folder containing aPackage named by aString.  Note that it
	doesn't hold onto a reference to the package, just the name, and
	if (at runtime) the package is not found then it'll default to image-relative
	handling"

	^ (super new)
		packageName: aString;
		yourself.
! !
!PackageRelativeFileLocator class categoriesFor: #package:!instance creation!public! !
!PackageRelativeFileLocator class categoriesFor: #packageNamed:!instance creation!public! !

PackageResourceLocator guid: (GUID fromString: '{4555894A-1EB4-4C71-A4F7-DBA92E37F0EE}')!
PackageResourceLocator comment: 'Copyright © Chris Uppal, 2002, 2005.
chris.uppal@metagnostic.org

PackageResourceLocator is a file locator that works relative to a specific sub-folder of the folder containing a named package.

If the file locator is used in a deployed app, then it falls back to looking in the same subfolder, but this time image-relative.

The point of the sub-folder aspect is that (when compared to my previous PackageRelativeFileLocator, which this is intended largely to replace) you can just say:

	locator := PackageResourceLocator package: aPackage.
	icon := Icon fromFile: ''MyIcon.ic'' usingLocator: locator.

rather than:

	locator := PackageRelativeFileLocator package: aPackage.
	icon := Icon fromFile: ''Resources\MyIcon.ico'' usingLocator: locator.

This has two advantages.  One is the relatively trivial saving in typing.  The more imporant one is that the resulting Icon will have an #identifier which is a simple name, ''MyIcon.ico'', rather than including the ''Resources\'' bit too.  This means that at runtime, if the icon is not found in the filesystem, the default icon loading will fall back to looking in the executable''s own resources for ''MyIcon.ico'', since that is a legitimage resource name, if you have added such a resource to your deployed executable, then it''ll be found.   The reason we have to avoid the ''Resources\'' bit is that ''Resources\MyIcon.ico'' is /not/ a legal resource name.

	-- chris'!
!PackageResourceLocator categoriesForClass!Unclassified! !
!PackageResourceLocator methodsFor!

basePath
	"since this stuff isn't bloody documented, I can only guess at what this method
	is *supposed* to do.  What it actually *does* is answer the resources sub-directory 
	of the directory where our package lives"

	| folder |

	folder := self package
			ifNil: [SessionManager current imageBase]
			ifNotNil: [:it | File splitPathFrom: it packageFileName].

	^ File composePath: folder subPath: subFolder.!

package
	"answer the Package relative to the location of which we will handle file paths or nil if the package
	doesn't exist or Package has been stripped out of the image"

	^ Smalltalk at: #Package ifPresent: [:it | it manager packageNamed: packageName ifNone: [nil]].!

packageName
	"answer the String name of the Package relative to the location of which we will handle file paths"

	^ packageName.!

packageName: aString
	"private -- set the String name of the Package relative to the location of which we will handle file paths"

	packageName := aString.!

printOn: aStream
	"append a developer friendly representation of ourself to the given stream.
	This implementation allows the reciever to be recreated (e.g. in an Inspector)"

	aStream
		display: self class;
		display: ' packageNamed: ';
		print: packageName.

	subFolder = self class defaultSubFolder ifFalse:
		[aStream
			display: ' subFolder: ';
			print: subFolder].


!

subFolder
	"answer the String name of sub-folder where we will look for resources"

	^ packageName.!

subFolder: aString
	"private -- set the String name of the sub-folder within which we will look for resources"

	subFolder := aString.! !
!PackageResourceLocator categoriesFor: #basePath!accessing!public! !
!PackageResourceLocator categoriesFor: #package!accessing!public! !
!PackageResourceLocator categoriesFor: #packageName!accessing!public! !
!PackageResourceLocator categoriesFor: #packageName:!initializing!private! !
!PackageResourceLocator categoriesFor: #printOn:!printing!public! !
!PackageResourceLocator categoriesFor: #subFolder!accessing!public! !
!PackageResourceLocator categoriesFor: #subFolder:!initializing!private! !

!PackageResourceLocator class methodsFor!

defaultSubFolder
	"answer the default sub-folder where we expect to find resources"

	^ 'Resources/'.!

package: aPackage
	"answer an instance that is configured to interpret files relative to
	the 'Resources/' sub-folder of the folder containing aPackage.
	It is an error if the package has not yet been saved.
	Note that it doesn't hold onto a reference to the package itself,
	just the name, and if (e.g. at runtime) the package is not found then
	it'll default to looking for the resoure in <image>/Resources/"

	^ self package: aPackage subFolder: 'Resources/'.
!

package: aPackage subFolder: aSubfolderName
	"answer an instance that is configured to interpret files relative to
	the named sub-folder of the folder containing aPackage.
	It is an error if the package has not yet been saved.
	Note that it doesn't hold onto a reference to the package itself,
	just the name, and if (e.g. at runtime) the package is not found then
	it'll default to looking for the resoure in named sub-folder of the image
	directory"

	^ self packageNamed: aPackage name subFolder: aSubfolderName.
!

packageNamed: aString
	"answer an instance that is configured to interpret files relative to
	the 'Resources/' sub-folder of the folder containing aPackage.
	Note that it doesn't hold onto a reference to the package itself,
	just the name, and if (e.g. at runtime) the package is not found then
	it'll default to looking for the resoure in <image>/Resources/"

	^ self packageNamed: aString subFolder: 'Resources/'.!

packageNamed: aString subFolder: aSubfolderName
	"answer an instance that is configured to interpret files relative to
	the named sub-folder of the folder containing aPackage.
	Note that it doesn't hold onto a reference to the package itself,
	just the name, and if (e.g. at runtime) the package is not found then
	it'll default to looking for the resoure in named subfolder of the image
	directory"

	^ (super new)
		packageName: aString;
		subFolder: aSubfolderName;
		yourself.
! !
!PackageResourceLocator class categoriesFor: #defaultSubFolder!public! !
!PackageResourceLocator class categoriesFor: #package:!instance creation!public! !
!PackageResourceLocator class categoriesFor: #package:subFolder:!instance creation!public! !
!PackageResourceLocator class categoriesFor: #packageNamed:!instance creation!public! !
!PackageResourceLocator class categoriesFor: #packageNamed:subFolder:!instance creation!public! !

"Binary Globals"!

"Resources"!

