MLTON := mlton
TARGET := build/june
MLB := src/june.mlb

.PHONY: all clean watch

all: $(TARGET)

$(TARGET): $(MLB)
	mkdir -p $(dir $@)
	$(MLTON) -output $@ $<

clean:
	rm -f $(TARGET)
