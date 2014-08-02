PROJECT = katja
PROJECT_VERSION = 0.5

DEPS = protobuffs
dep_protobuffs = git https://github.com/basho/erlang_protobuffs 0.8.1p4

TEST_DEPS = nifoc_ct_helper
dep_nifoc_ct_helper = git https://github.com/nifoc/nifoc_ct_helper master

ERLC_OPTS ?= -Werror +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

TEST_ERLC_OPTS ?= +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

CT_SUITES = eunit riemann
CT_OPTS = -ct_hooks nifoc_ct_hook []

ifneq ($(USER),travis)
	CT_OPTS += -cover ./test/cover.spec
endif

EDOC_OPTS = {def, [ \
					{years, "2014"}, \
					{version, "$(PROJECT_VERSION)"} \
				]}

proto_verbose_0 = @echo " PROTO " $(filter %.proto,$(?F));
proto_verbose = $(proto_verbose_$(V))

define compile_protobuffs
	$(proto_verbose) erl -noshell -pa ebin/ $(DEPS_DIR)/protobuffs/ebin/ -eval ' \
		Compile = fun(F) -> \
			protobuffs_compile:scan_file(F) \
		end, \
		_ = [Compile(F) || F <- string:tokens("$(1)", " ")], \
		init:stop()' > /dev/null
	$(eval proto_name := $(shell basename $(1) | sed s/\.[^\.]*$$//))
	@mkdir -p ebin/
	@mkdir -p include/
	@mv $(proto_name)_pb.beam ebin/
	@mv $(proto_name)_pb.hrl include/
endef

ebin/$(PROJECT).app:: $(shell find src -type f -name \*.proto 2>/dev/null)
	$(if $(strip $?),$(call compile_protobuffs,$?))

include erlang.mk

clean:: clean-proto

clean-proto:
	$(gen_verbose) rm -f $(shell find include -type f -name \*_pb.hrl 2>/dev/null)

upload-docs: docs
	$(gen_verbose) echo $(PROJECT_VERSION)
	rsync -avz --no-o --no-g -e ssh --chmod=og=r -p --delete --exclude '*.edoc' --exclude 'edoc-info' doc/ kempkens:/var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)
	ssh kempkens chown -R www-data:www-data /var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)

.PHONY: upload-docs
