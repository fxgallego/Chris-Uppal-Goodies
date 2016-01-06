| package |
package := Package name: 'CU Java Examples'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Just a small example of using JNIPort callbacks.  I may later add more examples to this package, but don''t hold your breath...

The Java source for class org.metagnostic.jniport.eg.TreeModelExample, which is
used by this example, is in the Docs\JNIPort folder.  It is also included in the
"Extras\JNIPort-Tests.zip" file, and is compiled as part of the "Extras\JNIPort-Tests.jar".
If you want to make use of the precompiled version then you should ensure that the
JAR file is on your Java classpath somewhere.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package classNames
	add: #StaticOMJEgTreeModelExample;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

StaticJavaLangObject subclass: #StaticOMJEgTreeModelExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StaticOMJEgTreeModelExample guid: (GUID fromString: '{18F196B8-A07D-4179-9FE9-31E1106C36EE}')!
StaticOMJEgTreeModelExample comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

'!
!StaticOMJEgTreeModelExample categoriesForClass!Unclassified! !
!StaticOMJEgTreeModelExample methodsFor!

getChildCountTag_null
	"answer the result of calling the receiver's public static getChildCountTag() Java method"

	^ self callObjectMethod: 'getChildCountTag' signature: '()Ljava/lang/Object;'.
!

getChildTag_null
	"answer the result of calling the receiver's public static getChildTag() Java method"

	^ self callObjectMethod: 'getChildTag' signature: '()Ljava/lang/Object;'.
!

getIndexOfChildTag_null
	"answer the result of calling the receiver's public static getIndexOfChildTag() Java method"

	^ self callObjectMethod: 'getIndexOfChildTag' signature: '()Ljava/lang/Object;'.
!

handleGetChild: aJavaArray
	"called from Java as the implementation of Object getChild(Object, int)"

	| name index classes class |

	name := aJavaArray at: 1.	"should be a java.Lang.String naming a class or '<<root>>'"
	name := name asString.

	index := aJavaArray at: 2.	"should be a java.lang.Integer (0-based)"
	index := index intValue_null + 1.

	"get list of subclasses"
	classes := name = '<<root>>'
			ifTrue: [Class allRoots]
			ifFalse: [(Smalltalk at: name) subclasses].

	"we want to return the name of the indexed subclass as a Java string"
	class := classes at: index ifAbsent: [^ nil].
	^ class name asJavaString: self jvm.
!

handleGetChildCount: aJavaLangString
	"called from Java as the implementation of int getChildCount(Object)"

	| name classes |

	name := aJavaLangString asString.

	"get list of subclasses"
	classes := name = '<<root>>'
			ifTrue: [Class allRoots]
			ifFalse: [(Smalltalk at: name) subclasses].

	"we want to return the number of subclasses as a java.lang.Integer"
	^ (jvm findClass: #'java.lang.Integer') new_int: classes size.
!

handleGetIndexOfChild: aJavaArray
	"called from Java as the implementation of Integer getIndexOfChild(Object, Object)"

	| parentName childName classes child index |

	parentName := aJavaArray at: 1.	"should be a java.Lang.String naming a class or '<<root>>'"
	parentName := parentName asString.

	childName := aJavaArray at: 2.	"should be a java.Lang.String naming a class"
	childName := childName asString.

	"get list of subclasses"
	classes := parentName = '<<root>>'
			ifTrue: [Class allRoots]
			ifFalse: [(Smalltalk at: parentName) subclasses].

	"find child"
	child := Smalltalk at: childName.
	index := classes indexOf: child.

	"we want to return the index (0-based) as a java.lang.Integer"
	^ (jvm findClass: #'java.lang.Integer') new_int: index-1.

!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

notifyRegistered
	"this is called by the class registry once we have been fully initialised.
	Normally that means that the instance class and static class are both
	correct and stable (will not change unless you, the programmer, manually
	create and register a new wrapper class that is more appropriate for the
	Java class we represent.  If ghost classes are in use then we are fully
	populated with ghost methods by the time this is called too"

	| callbackRegistry |

	callbackRegistry := self jvm callbackRegistry ifNil: [^ self].

	callbackRegistry
		setCallback: self getChildTag_null handler: [:it :params | self handleGetChild: params];
		setCallback: self getChildCountTag_null handler: [:it :params | self handleGetChildCount: params];
		setCallback: self getIndexOfChildTag_null handler: [:it :params | self handleGetIndexOfChild: params].
! !
!StaticOMJEgTreeModelExample categoriesFor: #getChildCountTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJEgTreeModelExample categoriesFor: #getChildTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJEgTreeModelExample categoriesFor: #getIndexOfChildTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJEgTreeModelExample categoriesFor: #handleGetChild:!callbacks!public! !
!StaticOMJEgTreeModelExample categoriesFor: #handleGetChildCount:!callbacks!public! !
!StaticOMJEgTreeModelExample categoriesFor: #handleGetIndexOfChild:!callbacks!public! !
!StaticOMJEgTreeModelExample categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticOMJEgTreeModelExample categoriesFor: #notifyRegistered!initializing!public! !

!StaticOMJEgTreeModelExample class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_null
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#getChildCountTag_null
		#getChildTag_null
		#getIndexOfChildTag_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.eg.TreeModelExample'.
! !
!StaticOMJEgTreeModelExample class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJEgTreeModelExample class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJEgTreeModelExample class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJEgTreeModelExample class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJEgTreeModelExample class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

"Binary Globals"!

"Resources"!

