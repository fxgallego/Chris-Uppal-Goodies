| package |
package := Package name: 'CU JNIPort Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

This package is empty except for a list of prerequisites that will load all the test cases for JNIPort
You can uninstall this package if you want, it will leave JNIPort and the tests installed.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base Tests';
	add: 'CU Java Callback Tests';
	add: 'CU Java Wrapper Generation Tests';
	yourself).

package setManualPrerequisites: #(
	'CU Java Base Tests'
	'CU Java Callback Tests'
	'CU Java Wrapper Generation Tests').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

