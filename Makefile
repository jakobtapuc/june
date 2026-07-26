MLTON := mlton
SMLFMT := smlfmt
TARGET := build/june
TARGET_CEK := build/june-cek
MLB := src/june.mlb
MLB_CEK := src/june-cek.mlb

.PHONY: all clean watch

all: $(TARGET) $(TARGET_CEK)

$(TARGET): $(MLB)
	mkdir -p $(dir $@)
	$(MLTON) -output $@ $<

$(TARGET_CEK): $(MLB_CEK)
	mkdir -p $(dir $@)
	$(MLTON) -codegen amd64 -profile no -output $@ $<

clean:
	rm -f $(TARGET)
	rm -f $(TARGET_CEK)

fmt:
	$(SMLFMT) --force \
		-allow-successor-ml true \
		-allow-opt-bar true \
		-allow-record-pun-exps true \
		$(MLB_CEK)
