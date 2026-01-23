#!/bin/bash
# Package finalization verification script

echo "=== Package Finalization Verification ==="
echo

# 1. Check docstrings on all public functions
echo "1. Checking docstrings on public functions..."
for file in doing-*.el; do
    # Find public functions (doing-XXX not doing--XXX)
    if grep -q "^(defun doing-[^-]" "$file" 2>/dev/null; then
        echo "  Checking $file..."
        # Extract function definitions and check if they have docstrings
        grep -A1 "^(defun doing-[^-]" "$file" | while read -r line; do
            if [[ "$line" =~ ^\(defun ]]; then
                func_name=$(echo "$line" | sed 's/^(defun \([^ (]*\).*/\1/')
                echo -n "    $func_name: "
            elif [[ "$line" =~ ^[[:space:]]*\" ]]; then
                echo "✓ has docstring"
            fi
        done
    fi
done
echo

# 2. Check autoload cookies on user-facing commands
echo "2. Checking autoload cookies..."
for file in doing-now.el doing-finish.el doing-cancel.el doing-current.el \
            doing-again.el doing-note.el doing-search.el doing-totals.el \
            doing-utils.el doing-view-commands.el; do
    if [ -f "$file" ]; then
        echo "  Checking $file..."
        # Get public functions
        funcs=$(grep "^(defun doing-[^-]" "$file" | sed 's/^(defun \([^ (]*\).*/\1/')
        for func in $funcs; do
            # Check if there's an autoload cookie before this function
            if grep -B1 "^(defun $func" "$file" | grep -q "^;;;###autoload"; then
                echo "    $func: ✓ has autoload"
            else
                echo "    $func: ✗ MISSING autoload"
            fi
        done
    fi
done
echo

# 3. Check package headers
echo "3. Checking package headers in doing.el..."
required_headers=("Author:" "Version:" "Package-Requires:" "Keywords:" "URL:")
for header in "${required_headers[@]}"; do
    if grep -q "^;; $header" doing.el; then
        echo "  ✓ $header"
    else
        echo "  ✗ MISSING: $header"
    fi
done
echo

# 4. Check byte compilation
echo "4. Checking byte compilation..."
make compile 2>&1 | grep -E "(Warning|Error)" > /tmp/compile-output.txt
if [ -s /tmp/compile-output.txt ]; then
    echo "  ✗ Compilation warnings/errors found:"
    cat /tmp/compile-output.txt
else
    echo "  ✓ Compiles without warnings"
fi
echo

# 5. Check tests
echo "5. Checking tests..."
make test 2>&1 | tail -3
echo

# 6. Check that internal files don't have autoload cookies
echo "6. Checking internal files have no autoload cookies..."
for file in doing-lib.el doing-rollover.el doing-view.el; do
    if grep -q "^;;;###autoload" "$file" 2>/dev/null; then
        echo "  ✗ $file has autoload cookies (should not)"
    else
        echo "  ✓ $file has no autoload cookies"
    fi
done
echo

echo "=== Verification Complete ==="
