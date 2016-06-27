PROJECT = katja
PROJECT_VERSION = 0.9.1

DEPS = protobuffs
dep_protobuffs = git https://github.com/basho/erlang_protobuffs 0.8.2

TEST_DEPS = nifoc_ct_helper
dep_nifoc_ct_helper = git https://github.com/nifoc/nifoc_ct_helper master

ifeq ($(USER),travis)
	TEST_DEPS += ecoveralls
	dep_ecoveralls = git https://github.com/nifoc/ecoveralls master
endif

ERLC_OPTS ?= -Werror +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

TEST_ERLC_OPTS ?= +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

# The otp_release calculated below will either be "R" followed by something, or
# an otp major version number starting from 17.
otp_release = $(shell erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), init:stop()')

# The rand module was introduced in OTP 18. This variable should be 0 if OTP
# version < 18, and 1 otherwise.
rand_module = $(shell echo $(otp_release) | grep -q -E "^17$$|^R" ; echo $$?)

ifeq ($(rand_module),1)
	ERLC_OPTS += -Drand_module=1
	TEST_ERLC_OPTS += -Drand_module=1
endif

CT_SUITES = eunit connection writer reader
CT_OPTS = -ct_hooks nifoc_ct_hook [] -cover ./test/cover.spec

EDOC_OPTS = {def, [ \
					{years, "2014-2016"}, \
					{version, "$(PROJECT_VERSION)"} \
				]}

ifneq ($(wildcard src/),)
proto_verbose_0 = @echo " PROTO " $(filter %.proto,$(?F));
proto_verbose = $(proto_verbose_$(V))

define compile_protobuffs
	$(proto_verbose) erl -noshell -pa ebin/ $(DEPS_DIR)/protobuffs/ebin/ -eval ' \
		Compile = fun(F) -> \
			protobuffs_compile:scan_file(F, [{compile_flags, [debug_info]}]) \
		end, \
		_ = [Compile(F) || F <- string:tokens("$(1)", " ")], \
		init:stop()' > /dev/null
	$(eval PROTONAME := $(shell basename $(1) | sed s/\.[^\.]*$$//))
	@mkdir -p ebin/
	@mkdir -p include/
	@mv $(PROTONAME)_pb.beam ebin/
	@mv $(PROTONAME)_pb.hrl include/
endef

# Keep `all' as the default target
all::

ebin/$(PROJECT).app:: $(shell find src -type f -name \*.proto 2>/dev/null)
	$(if $(strip $?),$(call compile_protobuffs,$?))
endif

include erlang.mk

clean:: clean-proto

clean-proto:
	$(gen_verbose) rm -f $(shell find include -type f -name \*_pb.hrl 2>/dev/null)

coverage-report: $(shell ls -1rt `find logs -type f -name \*.coverdata 2>/dev/null` | tail -n1)
	$(gen_verbose) erl -noshell -pa ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

upload-docs: docs
	$(gen_verbose) rsync -avz --no-o --no-g -e ssh --chmod=og=r -p --delete --exclude '*.edoc' --exclude 'edoc-info' doc/ kempkens.io:/iocage/jails/506fd9f8-15c0-11e5-adf5-477a0b920463/root/var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)
	@ssh kempkens.io chmod 755 /iocage/jails/506fd9f8-15c0-11e5-adf5-477a0b920463/root/var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)
	@ssh kempkens.io find /iocage/jails/506fd9f8-15c0-11e5-adf5-477a0b920463/root/var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION) -type d -exec chmod 755 {} +

.PHONY: clean-proto coverage-report upload-docs
