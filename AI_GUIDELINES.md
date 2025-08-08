# AI Guidelines for tasty-discover

This document provides guidelines and context for AI assistants working on the tasty-discover project.

## Project Overview

tasty-discover is a Haskell test discovery and runner tool for the Tasty testing framework. It automatically discovers and runs tests based on naming conventions.

### Key Technologies
- **Language**: Haskell (GHC 9.12.2+)
- **Build System**: Cabal
- **Testing Framework**: Tasty with multiple backends (HUnit, QuickCheck, Hedgehog, Hspec, etc.)
- **Preprocessing**: GHC's `-F -pgmF` mechanism for code generation

## Architecture

### Core Modules
- `Test.Tasty.Discover.Internal.Config` - CLI configuration and option parsing
- `Test.Tasty.Discover.Internal.Driver` - Main code generation logic
- `Test.Tasty.Discover.Internal.Generator` - Test type generators and boilerplate
- `app/Main.hs` - CLI entry point

### Key Features
- **Test Discovery**: Automatically finds tests based on prefixes (`unit_`, `prop_`, `spec_`, `test_`, `tasty_`, etc.)
- **Code Generation**: Creates test driver modules with main functions or exportable test trees
- **Custom Main Support**: `--no-main` option for custom test runners
- **Multiple Test Libraries**: Supports HUnit, QuickCheck, SmallCheck, Hedgehog, Hspec, and custom Tasty tests

## Development Practices

### Code Style
- Follow the guidelines in [CODING_STYLE.md](CODING_STYLE.md)
- Use qualified imports with meaningful aliases
- Prefer explicit type signatures
- Follow the existing module structure and naming conventions
- Avoid generating trailing whitespace on all lines in source files and documentation
- Use consistent indentation (spaces, not tabs)

### Testing
- Main test suite: `test/` directory using standard tasty-discover setup
- No-main demo: `test-no-main/` directory demonstrating `--no-main` feature
- Run tests with: `cabal test` or `cabal test <suite-name>`
- Build with: `cabal build`
- **Hybrid Testing**: Test suite uses both Hspec and Tasty tests via `tasty-hspec` integration
- **Expected Failures**: Use `tasty-expected-failure` package for tests that should fail until bugs are fixed

### Configuration Files
- `tasty-discover.cabal` - Package definition with multiple test suites
- `.hlint.yaml` - HLint configuration
- `.stylish-haskell.yaml` - Code formatting rules

### Git Best Practices
- **Always use `--no-pager`** when running git commands in terminal environments where pagers may cause issues
- Examples: `git --no-pager log`, `git --no-pager status`, `git --no-pager diff`
- This prevents git from hanging or showing incomplete output in automated environments
- Particularly important for AI assistants and CI/CD environments

## Common Tasks

### Adding New Test Prefixes
1. Add generator to `Generator.hs` in the `generators` list
2. Define prefix, imports, class (if needed), and setup function
3. Update README.md documentation
4. Add test cases to verify the new prefix works

### Modifying CLI Options
1. Update `Config.hs` with new option fields and parsing
2. Modify `Driver.hs` to use the new configuration
3. Update README.md and help text
4. Add tests to verify the option works correctly

### Code Generation Changes
1. Modify `Driver.hs` for output format changes
2. Test with both `--no-main` and regular modes
3. Ensure imports are properly managed (avoid redundant imports)
4. Verify generated code compiles and runs correctly

## Recent Changes

### --no-main Feature (2025)
- Added `--no-main` CLI option for custom test runners
- Generates modules with `tests` and `ingredients` exports instead of `main`
- Conditional import of `System.Environment` (only when main function is generated)
- Complete test suite in `test-no-main/` demonstrating usage

### Bug Fixes and Robustness Improvements (2025)
- Fixed backup file handling: changed file pattern from `*.hs*` to `*.hs`
- Added directory filtering in glob patterns to prevent crashes
- Introduced comprehensive regression testing for edge cases
- Fixed HLint warnings in generated code

### Symlink Issue (#38) and Expected Failure Testing (2025)
- Added reproduction test for symlink crash issue using `expectFail` from `tasty-expected-failure`
- Demonstrated hybrid testing approach: Hspec specs converted to Tasty via `tasty-hspec`
- **Important**: When tests need to expect failure, use Tasty's `expectFail` rather than Hspec's `xit`
- Created dedicated test project structure in `test-symlink-repro/` for issue reproduction

## Best Practices

### When Working with Generated Code
- Always test both `--no-main` and regular generation modes
- Check that imports are minimal and necessary
- Verify the generated code compiles without warnings
- Test with multiple test types (unit, property, custom, etc.)

### Error Handling
- Provide clear error messages for configuration issues
- Handle file system operations gracefully
- Use appropriate exit codes for CLI tool behavior

### Documentation
- Update README.md for user-facing changes
- Update CHANGELOG.md following Keep a Changelog format
- Include examples for new features
- Maintain consistency between code and documentation

### Regression Testing Strategy
- Create specific test directories for reproducing reported bugs (e.g., `test/BackupFiles/`, `test/ModulesGlob/`)
- Add regression tests with descriptive names that reference the issue being fixed
- Include both positive and negative test cases in the test data
- Update cabal file to include new test modules when adding test directories

### Expected Failure Testing
- Use `tasty-expected-failure` package for tests that reproduce known bugs
- Prefer `expectFail` from Tasty over Hspec's `xit` when working in hybrid test environments
- Create both failing reproduction tests and placeholder tests in mixed Hspec/Tasty suites
- Document the GitHub issue number and expected behavior in test comments
- **Key Insight**: `xit` in Hspec becomes pending in Tasty via `tasty-hspec`, but still shows as failure in test suite

### Code Quality and Safety
- Use unsafe functions only when strong invariants guarantee safety
- Document why unsafe functions are necessary and when they should/shouldn't be used
- Add detailed comments explaining the safety invariants
- Fix HLint warnings in generated code to maintain code quality standards
- **Avoid generating trailing whitespace**: Ensure no lines end with spaces or tabs in source code, documentation, or generated code
- Configure your editor to show/remove trailing whitespace automatically

### File System Robustness
- Filter file operations to handle edge cases (directories, backup files, etc.)
- Use appropriate file existence checks before processing
- Handle glob patterns that might match unintended file types
- Consider what happens when file patterns match both files and directories

## Testing Framework Integration

### Hybrid Hspec/Tasty Architecture
The test suite uses a hybrid approach where Hspec specs are converted to Tasty tests via `tasty-hspec`. This creates some important considerations:

- **Test Discovery**: Both Hspec `spec_*` functions and Tasty `tasty_*` functions are discovered
- **Conversion**: Hspec specs become Tasty tests through `HS.testSpec`
- **Failure Handling**: Hspec's `xit` becomes "pending" in Tasty but still counts as test suite failure

### Expected Failure Patterns
When creating tests for known bugs or incomplete features:

1. **For pure Tasty tests**: Use `expectFail` from `tasty-expected-failure`
2. **For Hspec tests in hybrid environment**: Create both a placeholder Hspec test and a proper Tasty test with `expectFail`
3. **Documentation**: Always include GitHub issue numbers and expected behavior

### Example Pattern for Bug Reproduction:
```haskell
-- Placeholder Hspec test
spec_bugReproduction :: Spec
spec_bugReproduction = describe "Bug reproduction" $ do
  it "this test is disabled - see tasty_bugReproduction instead" $ do
    pure () :: IO ()

-- Actual test with expectFail
tasty_bugReproduction :: IO T.TestTree
tasty_bugReproduction = do
  pure $ expectFail $ HU.testCase "reproduces GitHub issue #XX" $ do
    -- Test implementation that should fail until bug is fixed
```

## File Patterns

### Test Discovery
- Test files should be in the configured search directory (default: same as source file)
- Test modules should export functions with recognized prefixes
- Use glob patterns for include/exclude functionality

### Generated Code
- Generated modules include appropriate language extensions
- Import statements are sorted and deduplicated
- Code follows consistent formatting patterns
- Generated functions have meaningful names and documentation
- No trailing whitespace in generated output

## Debugging

### Common Issues
- **Missing imports**: Check generator import lists in `Generator.hs`
- **Wrong test prefix**: Verify the prefix matches exactly in the generators list
- **Build failures**: Ensure all dependencies are listed in cabal file
- **Runtime errors**: Check that generated main function handles arguments correctly
- **File discovery problems**: Verify glob patterns and file filtering logic
- **Backup file interference**: Ensure test discovery ignores `.orig`, `.bak` files
- **Directory matching**: Check that directory entries are filtered out from file operations
- **Expected failure confusion**: In hybrid Hspec/Tasty environments, use `expectFail` not `xit` for true expected failures
- **cabal.project configuration**: Ensure `packages: .` is present for proper cabal commands

### Development Patterns from Recent Changes
- **Issue-driven development**: Create regression tests for reported GitHub issues
- **Incremental safety**: Add safety checks and filters rather than rewriting large portions
- **Documentation-first**: Update documentation alongside code changes
- **Test data organization**: Create dedicated test directories for specific scenarios

### Useful Commands
```bash
# Build and test everything
cabal build && cabal test

# Test specific suite
cabal test no-main-test

# Debug test discovery
cabal run tasty-discover -- --debug [options] file.hs _ ModuleName

# Check generated code
cat dist-newstyle/build/.../generated-file.hs

# Git commands (use --no-pager to avoid pager issues in terminals)
git --no-pager log --oneline -10
git --no-pager status
git --no-pager diff
git --no-pager show
```

## Contributing Guidelines

1. **Understand the change**: Test discovery, code generation, or CLI interface?
2. **Follow patterns**: Look at existing generators/options for similar functionality
3. **Test thoroughly**: Both positive and negative test cases
4. **Update docs**: README.md, CHANGELOG.md, and help text
5. **Verify backwards compatibility**: Existing test suites should continue working

### Lessons from Recent Development

**When fixing bugs:**
- Create minimal reproducible test cases in dedicated directories
- Change implementation incrementally rather than large rewrites
- Add safety filters and checks rather than removing functionality
- Verify the fix doesn't break existing functionality

**When adding features:**
- Start with a clear use case and example usage
- Consider both the generated code and the CLI interface impact
- Add comprehensive documentation and examples
- Create a complete test suite demonstrating the feature

**When working with test frameworks:**
- Understand the distinction between Hspec and Tasty test behaviors
- Use appropriate expected failure mechanisms (`expectFail` vs `xit` vs `pendingWith`)
- Be aware that `tasty-hspec` converts Hspec tests to Tasty, affecting failure behavior
- Consider both standalone test behavior and integration into larger test suites

**When improving robustness:**
- Identify edge cases through real-world usage patterns
- Add defensive programming practices (file existence checks, type filtering)
- Document assumptions and invariants clearly
- Use regression tests to prevent future breakage

---

*This document should be updated whenever significant architectural changes are made to the project.*
