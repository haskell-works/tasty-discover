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

### Testing
- Main test suite: `test/` directory using standard tasty-discover setup
- No-main demo: `test-no-main/` directory demonstrating `--no-main` feature
- Run tests with: `cabal test` or `cabal test <suite-name>`
- Build with: `cabal build`

### Configuration Files
- `tasty-discover.cabal` - Package definition with multiple test suites
- `.hlint.yaml` - HLint configuration
- `.stylish-haskell.yaml` - Code formatting rules

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

## Debugging

### Common Issues
- **Missing imports**: Check generator import lists in `Generator.hs`
- **Wrong test prefix**: Verify the prefix matches exactly in the generators list
- **Build failures**: Ensure all dependencies are listed in cabal file
- **Runtime errors**: Check that generated main function handles arguments correctly

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
```

## Contributing Guidelines

1. **Understand the change**: Test discovery, code generation, or CLI interface?
2. **Follow patterns**: Look at existing generators/options for similar functionality
3. **Test thoroughly**: Both positive and negative test cases
4. **Update docs**: README.md, CHANGELOG.md, and help text
5. **Verify backwards compatibility**: Existing test suites should continue working

---

*This document should be updated whenever significant architectural changes are made to the project.*
