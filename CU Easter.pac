| package |
package := Package name: 'CU Easter'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2003.
chris.uppal@metagnostic.org

A few loose methods added to Date class for calculating Western Christian Easter in various years.  This code is based on an algorithm that is plastered all over the Web.  See there for references ;-)   The above copyright notice should be taken with a pinch of salt -- I invented neither the algorithm nor its expression.'.

package basicPackageVersion: '1.00'.


package methodNames
	add: 'Date class' -> #easter:;
	add: 'Date class' -> #easterMonday;
	add: 'Date class' -> #easterMonday:;
	add: 'Date class' -> #easterSunday;
	add: 'Date class' -> #easterSunday:;
	add: 'Date class' -> #goodFriday;
	add: 'Date class' -> #goodFriday:;
	add: 'Date class' -> #shroveTuesday;
	add: 'Date class' -> #shroveTuesday:;
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

!Date class methodsFor!

easter: aYear
	"Private - Answer an instance that represents the date of Easter Sunday in the year given
	by the <Integer> aYear.  The calculation returns Western Christianity's idea of Easter
	as defined by the Council of Nicea.  We also use the Gregorian calendar for all dates.
	This calculation is apparently valid for years >= 1583"

	"
		self assert: [(self easter: 2002) displayString = '31 March 2002'].
		self assert: [(self easter: 2003) displayString = '20 April 2003'].
		self assert: [(self easter: 2004) displayString = '11 April 2004'].
		self assert: [(self easter: 2005) displayString = '27 March 2005'].
		self assert: [(self easter: 2006) displayString = '16 April 2006'].
		self assert: [(self easter: 2007) displayString = '08 April 2007'].
		self assert: [(self easter: 2008) displayString = '23 March 2008'].
		self assert: [(self easter: 2009) displayString = '12 April 2009'].
		self assert: [(self easter: 2010) displayString = '04 April 2010'].
	"

	| x a b c d e f g h i k l m month day |

#CUadded.

	a := aYear \\ 19.
	b := aYear // 100.
	c := aYear \\ 100.
	d := b // 4.
	e := b \\ 4.
	f := (b + 8) // 25.
	g := (b - f + 1) // 3.
	h := ( (19*a) + b - d - g + 15 ) \\ 30.
	i := c // 4.
	k := c \\ 4.
	l := (32 + (2*e) + (2*i) - h - k) \\ 7.
	m := (a + (11*h) + (22*l) ) // 451.
	x := h + l - (7*m) + 114.
	month := x // 31.		"1-based number of the month (Jan = 1)"
	day := x \\ 31.		"0-based number of the day in the month"

	
	^ self
		newDay: day + 1
		monthIndex: month
		year: aYear.!

easterMonday
	"Answer an instance that represents the date of Easter Monday in the current year."

	^ self easterMonday: (self today year).!

easterMonday: aYear
	"Answer an instance that represents the date of Easter Monday in the year given
	by the <Integer> aYear."

	^ (self easter: aYear) addDays: 1.!

easterSunday
	"Answer an instance that represents the date of Easter Sunday in the current year."

	^ self easterSunday: (self today year).!

easterSunday: aYear
	"Answer an instance that represents the date of Easter Sunday in the year given
	by the <Integer> aYear."

	^ self easter: aYear.!

goodFriday
	"Answer an instance that represents the date of Good Friday in the current year."

	^ self goodFriday: (self today year).!

goodFriday: aYear
	"Answer an instance that represents the date of Good Friday in the year given
	by the <Integer> aYear."

	^ (self easter: aYear) addDays: -2.!

shroveTuesday
	"Answer an instance that represents the date of Shrove Tuesday (aka Pancake Day) in the
	current year."

	^ self shroveTuesday: (self today year).!

shroveTuesday: aYear
	"Answer an instance that represents the date of Shrove Tuesday (aka Pancake Day) in the year given
	by the <Integer> aYear."

	^ (self easter: aYear) addDays: -47.! !
!Date class categoriesFor: #easter:!date calculations!instance creation!private! !
!Date class categoriesFor: #easterMonday!date calculations!instance creation!public! !
!Date class categoriesFor: #easterMonday:!date calculations!instance creation!public! !
!Date class categoriesFor: #easterSunday!date calculations!instance creation!public! !
!Date class categoriesFor: #easterSunday:!date calculations!instance creation!public! !
!Date class categoriesFor: #goodFriday!date calculations!instance creation!public! !
!Date class categoriesFor: #goodFriday:!date calculations!instance creation!public! !
!Date class categoriesFor: #shroveTuesday!date calculations!instance creation!public! !
!Date class categoriesFor: #shroveTuesday:!date calculations!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

