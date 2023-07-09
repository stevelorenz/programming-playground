GROOVY := groovy

define run
	@echo "- Run $1"
	@$(GROOVY) $1
endef

all:
	@echo "* Run all sample programs..."

	@$(call run,./chap02/range_usage.groovy)
	@$(call run,./chap02/map_usage.groovy)
	@$(call run,./chap02/gstring.groovy)
	@$(call run,./chap02/BookBean.groovy)
	@$(call run,./chap02/ImmutableBook.groovy) > /dev/null
