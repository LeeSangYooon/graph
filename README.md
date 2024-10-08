# graph
## 목적
수식을 입력받고 컴파일해서 그래프를 띄움
## 원리
파싱 -> 구문 분석 -> 바이트 코드 생성 -> 각 함수값을 가상 머신에서 계산해서 그래프 생성   
타입이 숫자 하나라서 의미 분석은 생략함 
가상머신으로 그래프를 그리는 코드:
```
for (x <- 0 until width) {
    val xValue = (x.toFloat / width - 0.5) * range * 2
    val prompt = Code(List(ByteCode.PUSH(xValue), ByteCode.Call(name)))
    val yValue = vm.run(prompt).memory.stack.last
    val y = ((-yValue / range + 1) / 2 * height).round.toInt
    if(0 <= y && y < height) {
        drawCircle(x, y, 4, lineColor)
    }
}
```


## 사용법
식을 입력하면 그 값을 계산해서 출력하고 함수를 정의하면 그래프를 그려서 outputs 폴더에 저장함. 
변수가 1개인 함수는 y=f(x)의 그래프를 그리고, 변수가 2개인 함수는 평면 상에 함수값을 색상으로 표시함.
푸른색이 양의 값이고 붉은색이 음의 값임

주의: 음수 리터럴을 구현하지 않아서 0-1 처럼 써야함 

### 조건문
줄바꿈은 자유롭고 다음과 같이 씀
```
filter(x) = if (x>2) {2} elif (x<0-2) {0-2} else {0}
```

### 기본 정의 함수, 상수
```
pi = 3.1415...
e = 2.7182...
sin(x)
cos(x)
tan(x)
ln(x)
abs(x)
a+b
a-b
a*b
a/b
a^b (지수)
```

## 예시
### 2차원 그래프
#### 삼차함수
`f(x) = x * (x-1) * (x + 1)`
![2024_8_10_21_53 f.png](outputs%2F2024_8_10_21_53%20f.png)
#### sin함수
`f(x) = sin(pi * x)`
![2024_8_10_21_57 SIN.png](outputs%2F2024_8_10_21_57%20SIN.png)
### 3차원 그래프
#### 원
```
filter(x) = if (x>0.1) {2} elif (x<0-0.1) {0-2} else {0}
f(x,y)= filter((x-1)^2+y^2 - 1)
```
![2024_8_10_22_20 f.png](outputs%2F2024_8_10_22_20%20f.png)
#### 쌍곡선
`f(x,y) = x^2 - y^2 - 1`
![2024_8_10_21_58 f.png](outputs%2F2024_8_10_21_58%20f.png)
#### 삼각함수 관련 그래프
```
filter(x) = if (x>0.1) {2} elif (x<0-0.1) {0-2} else {0}
f(x, y) = filter(sin((x*x*4)^(y*2)) + cos((x*x*4)^(y*2)))
```
![2024_8_10_22_19 f.png](outputs%2F2024_8_10_22_19%20f.png)
`f(x,y) = sin(x) + sin(y)`
![2024_8_10_21_58 asdf.png](outputs%2F2024_8_10_21_58%20asdf.png)
`f(x,y) = sin(pi * x) - cos(pi * y)`
![2024_8_10_21_59 dfjkj.png](outputs%2F2024_8_10_21_59%20dfjkj.png)
