# sml-wav build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML (use-and-run; no link step)
#   make all-tests  run the suite under both compilers
#   make example    synthesize the demo .wav + render waveform/spectrum PNGs
#   make clean      remove build artifacts
#
# Layout B (dependent): own sources live in src/; sml-fft and its dependency
# sml-complex are vendored under lib/ and loaded first, in dependency order.
# The example additionally vendors sml-plot (+ its sml-font/raster/image/color/
# inflate stack) to render the waveform and FFT spectrum to PNG.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
LIBDIR     := lib/github.com/sjqtentacles
CPLXDIR    := $(LIBDIR)/sml-complex
FFTDIR     := $(LIBDIR)/sml-fft
INFLATEDIR := $(LIBDIR)/sml-inflate
COLORDIR   := $(LIBDIR)/sml-color
IMAGEDIR   := $(LIBDIR)/sml-image
RASTERDIR  := $(LIBDIR)/sml-raster
FONTDIR    := $(LIBDIR)/sml-font
PLOTDIR    := $(LIBDIR)/sml-plot
TEST_MLB   := test/test.mlb
SRCS       := $(wildcard $(CPLXDIR)/* $(FFTDIR)/* src/* test/*.sml) $(TEST_MLB)
EXSRCS     := $(wildcard $(INFLATEDIR)/* $(COLORDIR)/* $(IMAGEDIR)/* \
                $(RASTERDIR)/* $(FONTDIR)/* $(PLOTDIR)/*)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

example: $(BIN)/demo
	mkdir -p assets
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) $(EXSRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

# Poly/ML has no native .mlb support; the suite runs at top level and exits on
# its own.  Load the vendored sml-complex + sml-fft first, then the wav
# sources, then the test driver.
poly test-poly:
	printf 'use "$(CPLXDIR)/complex.sig";\nuse "$(CPLXDIR)/complex.sml";\nuse "$(FFTDIR)/fft.sig";\nuse "$(FFTDIR)/fft.sml";\nuse "src/wav.sig";\nuse "src/wav.sml";\nuse "test/harness.sml";\nuse "test/support.sml";\nuse "test/test_io.sml";\nuse "test/test_osc.sml";\nuse "test/test_dsp.sml";\nuse "test/test_adsr.sml";\nuse "test/test_filter.sml";\nuse "test/entry.sml";\nuse "test/main.sml";\n' | $(POLY) -q --error-exit

all-tests: test test-poly

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -f $(BIN)/test-mlton $(BIN)/demo
