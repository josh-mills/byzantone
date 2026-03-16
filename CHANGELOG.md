# Changelog

## v1.4.0 (2026-03-16)
### Added
 - Change log with release notes view accessible from the application menu

## v1.3.1 (2026-03-09)
 ### Fixed
 - Audio session type was preventing audio capture
  
## v1.3.0 (2026-03-08)
 ### Added
 - Collapsible header for space optimization on mobile
 ### Changed
 - Tighter accidental buttons at mobile breakpoints
 - Set audio session type to prevent device muting from preventing playback

## v1.2.0 (2026-03-04)
### Added
 - Option to set a (bounded) arbitrary frequency for a reference Δι
 - Detected pitch continuity, so that when a frequency is not detected, the previous one is kept for a while
### Removed
 - Pitch detection spectrogram visualization no longer renders on mobile layout (i.e., when it would be hidden anyway)
 
## v1.1.0 (2026-02-21)
### Added
 - Numeric feedback on detected pitch relative to nearest pitch
 - Users can select the unit of the feedback (moria offset, cents offset, or absolute frequency)
### Changed
 - Pitch detection now detects register dynamically rather than assuming bass register as the default 
### Fixed
 - Detected pitch indicator is no longer slightly offset from the pitch center
 
## v1.0.5 (2026-01-27)
### Added
 - Introduces `elm-review` to improve code quality
### Changed
 - Refactors in response to `elm-review`
 
## v1.0.4 (2026-01-18)
### Changed
 - Refactors to strengthen type safety, improve maintainability, and reduce future bug opportunities
 
## v1.0.3 (2026-01-03)
### Fixed
 - Applying a sharp to the bottom interval of a range no longer causes a change to the visible range
 
## v1.0.2 (2026-01-03)
### Fixed
 - Applying an accidental to an interval now successfully applies the accidental to the resulting pitch

## v1.0.1 (2025-11-24)
### Changed
 - Lazy rendering for intervals (minor performance improvements)
 
## v1.0.0 (2025-09-26)
### Added
 - New mobile-first UX provides 
 - Helpful new icons should be helpful
### Changed
 - Initial render is based on viewport information loaded upon app initialization rather than an arbitrary default
 - Slight tweak to auto layout logic
### Removed
 - Controls that were previously always visible are now housed in collapsible menus for progressive disclosure pattern

## v0.7.1 (2025-09-04)
### Added
 - Multiple accidentals can be applied simultaneously in Listen mode
 - Cancel accidental button to cancel a currently applied accidental
### Changed
 - Refactors for improved modularity
### Fixed
 - Pitch tracking now matches against inflected pitches
 
## v0.7.0 (2025-08-15)
### Added
 - Users can select either "Listen" or "Play" audio mode, which introduces pitch tracker capabilities
 - Detected pitch is plotted in the pitch space view, with color indicating closeness to a target pitch
 - Users can select register for pitch detection

## v0.6.3 (2025-08-11)
### Changed
 - More robust derived state persistence and data structures for improved performance
 - Greater use of lazy rendering
 
## v0.6.2 (2025-06-28)
### Changed
 - Minor improvements to audio engine processing
 
## v0.6.1 (2025-06-25)
### Changed
 - Lazy rendering for improved performance
 
## v0.6.0 (2025-06-18)
### Added
 - Users can now apply accidentals to pitches and intervals

## v0.5.0 (2025-04-02)
### Added
 - Users can now select an Ison
### Changed
 - Style tweaks for button elements
 - Improved data management avoids re-computation for a minor performance improvement

## v0.4.2 (2025-03-27)
### Fixed
 - GitHub Actions script no longer attempts to deploy unless push is to `main`

## v0.4.1 (2025-03-27)
### Changed
 - Refactors to model management for improved maintainability

## v0.4.0 (2025-03-25)
### Added
 - Users can now set the range of the scale as it appears in the visible range
### Changed
 - Set default orientation of layout to vertical, and adjust with viewport changes
 - Improved style management and consistency

## v0.3.0 (2025-03-04)
### Added
 - Create a Pitch Standard concept (select between Νη = 256 Hz and Κε = 440 Hz)
### Changed
 - Created abstractions around radio fieldset UX pattern

## v0.2.1 (2025-02-26)
### Changed
 - Upgrade TailwindCSS framework
 
## v0.2.0 (2025-02-10)
### Added
 - Create Settings and About modal
 - Playback register can now be set
 
## v0.1.0 (2025-01-16)
### Added
 - GitHub actions for static hosting on GitHub Pages (effective initial release)

---

# Changelog Format Guidelines

This changelog follows the [Keep a Changelog](https://keepachangelog.com/) format. When adding entries, please follow these guidelines:

## Format Structure

```markdown
## vM.m.p (yyyy-mm-dd)
### Added
- New features

### Changed  
- Changes in existing functionality

### Fixed
- Bug fixes

### Deprecated
- Soon-to-be removed features

### Removed
- Removed features

### Security
- Security improvements
```

## Versioning

- Bump patch version for trivial things like bug fixes, visual tweaks, copy changes, pure refactors
- Bump minor version for new features or meaningful additions that don't disrupt existing workflows
- Bump major version for significant changes in the UX, or meaningful breakages to existing usages

## Change Types

- **Added** - New features
- **Changed** - Changes in existing functionality  
- **Deprecated** - Soon-to-be removed features
- **Removed** - Removed features
- **Fixed** - Bug fixes
- **Security** - Security improvements

## Notes

- Include date in YYYY-MM-DD format for all releases
- Use clear, concise descriptions
- Group related changes under appropriate categories
- This section is not parsed by the changelog system
