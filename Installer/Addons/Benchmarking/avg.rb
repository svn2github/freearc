
lines = []
cnt = Hash.new(0)
sum = Hash.new
rescnt = Hash.new

def abort(a)
  for line in a
    p line
  end
  exit 1
end

# Читаем

  for str in ARGF
    str = str.chomp.chomp

    # Определяем заголовок/архиватор/режим/результаты: header, archiver, mode, results
    if str==""               then lines <<= "" unless lines[-1]==""; next; end
    if str.include? "bytes"  then header=str; next; end
    if str[0]!=32            then archiver=str; next; end

    mode, result  =  (/^(.*?) (\d.*)$/. match str)[1,2]
    mode = mode.strip
    results = result.split.map{|x|x.to_f}

    # Добавляем архиватор+режим в список, если их там раньше не было
    line = [archiver, mode]
    lines <<= line  unless lines.include?(line)
    cnt[line] += 1

    # Проверяем, что количество элементов результата не зависит от заголовка
    rescnt[line] ||= results.length
    if rescnt[line] != results.length  then abort ["Different amounts of results for:", line]; end

    # Суммируем данные независимо от заголовка
    results.each_index  { |i|
      sum[archiver]          ||= {}
      sum[archiver][mode]    ||= []
      sum[archiver][mode][i] ||= 1.0
      sum[archiver][mode][i] *= results[i]
    }
  end

# Проверяем

  i1 = cnt.keys[0]
  n1 = cnt[i1]
  for i,n in cnt
    if n != n1 then abort ["Different amounts of lines:", {i1=>n1}, {i=>n}]; end
  end

# Печатаем

  last_archiver = ''
  for archiver, mode in lines
    if archiver==""               then puts; next; end
    if archiver != last_archiver  then puts archiver; last_archiver = archiver; end
    printf " %-14s%s\n", mode, sum[archiver][mode]. map {|x| format " %6.3f", x**(1.0/n1)}
  end

