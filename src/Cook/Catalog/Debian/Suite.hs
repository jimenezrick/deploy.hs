module Cook.Catalog.Debian.Suite where

data Release = Jessie | Stretch
data Dist = Stable | Testing | Unstable

data Suite = Dist Dist | Release Release

jessie, stretch, testing, stable, unstable :: Suite
jessie = Release Jessie
stretch = Release Stretch
stable = Dist Stable
testing = Dist Testing
unstable = Dist Unstable

showSuite :: Suite -> String
showSuite (Release Jessie) = "jessie"
showSuite (Release Stretch) = "stretch"
showSuite (Dist Stable) = "stable"
showSuite (Dist Testing) = "testing"
showSuite (Dist Unstable) = "unstable"
