.PHONY: format
format:
	@echo "* Format all sample Groovy programs..."
	@find ./ -type f -name '*.groovy' -exec npm-groovy-lint --format \; > /dev/null 2>&1

.DEFAULT_GOAL := format
