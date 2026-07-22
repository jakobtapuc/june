MLTON := mlton
SMLFMT := smlfmt
TARGET := build/june
MLB := src/june.mlb

.PHONY: all clean watch

all: $(TARGET)

$(TARGET): $(MLB)
	mkdir -p $(dir $@)
	$(MLTON) -output $@ $<

clean:
	rm -f $(TARGET)

fmt:
	$(SMLFMT) --force \
		-allow-successor-ml true \
		-allow-opt-bar true \
		-allow-record-pun-exps true \
		$(MLB)