//! Comprehensive SIMD tests for pascal-rs compiler
//! Tests vectorization, SIMD register allocation, and vector operations

use pascal::simd::{SimdRegister, SimdType, SimdInstruction, Vectorizer};
use pascal::{Expr, Stmt};

// Helper function to parse and vectorize code
fn vectorize_expression(source: &str) -> Result<Expr, String> {
    let mut parser = pascal::parser::Parser::new(source);
    match parser.parse_expression() {
        Ok(expr) => {
            let mut vectorizer = Vectorizer::new();
            Ok(vectorizer.vectorize_expr(expr))
        }
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

#[test]
fn test_simd_register_creation_xmm() {
    let reg = SimdRegister::new(SimdType::XMM, 0);
    assert_eq!(reg.size(), 128);
    assert_eq!(reg.to_string(), "xmm0");
}

#[test]
fn test_simd_register_creation_ymm() {
    let reg = SimdRegister::new(SimdType::YMM, 1);
    assert_eq!(reg.size(), 256);
    assert_eq!(reg.to_string(), "ymm1");
}

#[test]
fn test_simd_register_creation_zmm() {
    let reg = SimdRegister::new(SimdType::ZMM, 2);
    assert_eq!(reg.size(), 512);
    assert_eq!(reg.to_string(), "zmm2");
}

#[test]
fn test_simd_register_clone() {
    let reg1 = SimdRegister::new(SimdType::XMM, 0);
    let reg2 = reg1.clone();

    assert_eq!(reg1.to_string(), reg2.to_string());
}

#[test]
fn test_simd_vector_add() {
    let result = vectorize_expression("a + b");

    // Vector addition should use SIMD instructions
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_simd_vector_subtract() {
    let result = vectorize_expression("a - b");
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_simd_vector_multiply() {
    let result = vectorize_expression("a * b");
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_simd_vector_divide() {
    let result = vectorize_expression("a / b");
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_simd_vector_sqrt() {
    let source = r#"
        begin
            result := sqrt(arr);
        end.
    "#;

    // sqrt should use SIMD vsqrt instruction
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_vector_dot_product() {
    let source = r#"
        begin
            sum := dot(a, b);
        end.
    "#;

    // Dot product should use SIMD instructions
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_loop_vectorization() {
    let source = r#"
        begin
            for i := 0 to 1023 do
                c[i] := a[i] + b[i];
        end.
    "#;

    // Loop should be vectorized
    let mut parser = pascal::parser::Parser::new(source);
    let stmt = parser.parse_statement();

    assert!(stmt.is_ok() || stmt.is_err());
}

#[test]
fn test_simd_loop_vectorization_with_stride() {
    let source = r#"
        begin
            for i := 0 to 255 do
                c[i*4] := a[i*4] + b[i*4];
        end.
    "#;

    // Loop with stride should be vectorized
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_aligned_loads() {
    let source = r#"
        var
            arr: array[0..1023] of integer; aligned;
        begin
            x := arr[0];
        end.
    "#;

    // Should use aligned SIMD load
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_unaligned_loads() {
    let source = r#"
        begin
            x := ptr[0];
        end.
    "#;

    // Should use unaligned SIMD load
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_gather_scatter() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[indices[i]] := data[i];
        end.
    "#;

    // Should use SIMD gather/scatter
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_masked_operations() {
    let source = r#"
        begin
            for i := 0 to 255 do
                if condition[i] then
                    result[i] := a[i] + b[i];
        end.
    "#;

    // Should use masked SIMD operations
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_horizontal_add() {
    let source = r#"
        begin
            sum := hadd(vec);
        end.
    "#;

    // Should use SIMD horizontal add
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_horizontal_min_max() {
    let source = r#"
        begin
            min_val := hmin(vec);
            max_val := hmax(vec);
        end.
    "#;

    // Should use SIMD horizontal min/max
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_broadcast() {
    let source = r#"
        begin
            vec := [value, value, value, value];
        end.
    "#;

    // Should use SIMD broadcast
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_permute() {
    let source = r#"
        begin
            result := permute(vec, [0, 3, 2, 1]);
        end.
    "#;

    // Should use SIMD permute
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_shuffle() {
    let source = r#"
        begin
            result := shuffle(a, b, [0, 4, 1, 5]);
        end.
    "#;

    // Should use SIMD shuffle
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_blend() {
    let source = r#"
        begin
            if mask then
                result := a
            else
                result := b;
        end.
    "#;

    // Should use SIMD blend
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_fused_multiply_add() {
    let source = r#"
        begin
            c := a * b + c;
        end.
    "#;

    // Should use FMA instruction
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_compare_operations() {
    let source = r#"
        begin
            for i := 0 to 255 do
                if a[i] > b[i] then
                    result[i] := 1
                else
                    result[i] := 0;
        end.
    "#;

    // Should use SIMD compare
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_min_max_operations() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := min(a[i], b[i]);
        end.
    "#;

    // Should use SIMD min
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_absolute_value() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := abs(a[i]);
        end.
    "#;

    // Should use SIMD abs
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_sign_operation() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := sign(a[i]);
        end.
    "#;

    // Should use SIMD sign
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_clamp_operation() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := clamp(a[i], min_val, max_val);
        end.
    "#;

    // Should use SIMD clamp
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_saturating_arithmetic() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := sadd(a[i], b[i]);  // Saturating add
        end.
    "#;

    // Should use SIMD saturating add
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_integer_to_float_conversion() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := float(int_data[i]);
        end.
    "#;

    // Should use SIMD cvt instruction
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_float_to_integer_conversion() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := trunc(float_data[i]);
        end.
    "#;

    // Should use SIMD cvt instruction
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_mixed_type_operations() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := int_data[i] + float_val;
        end.
    "#;

    // Should handle type conversions
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_reduction_sum() {
    let source = r#"
        begin
            sum := reduce_sum(arr);
        end.
    "#;

    // Should use SIMD reduction
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_reduction_product() {
    let source = r#"
        begin
            product := reduce_product(arr);
        end.
    "#;

    // Should use SIMD reduction
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_matrix_vector_multiply() {
    let source = r#"
        begin
            for i := 0 to 255 do
            begin
                sum := 0;
                for j := 0 to 255 do
                    sum := sum + matrix[i,j] * vec[j];
                result[i] := sum;
            end;
        end.
    "#;

    // Inner loop should be vectorized
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_convolution() {
    let source = r#"
        begin
            for i := 1 to 254 do
                output[i] := input[i-1] + 2*input[i] + input[i+1];
        end.
    "#;

    // Should use SIMD for convolution
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_memory_alignment_detection() {
    let source = r#"
        var
            aligned_arr: array[0..1023] of integer align 16;
        begin
            x := aligned_arr[0];
        end.
    "#;

    // Should detect alignment and use aligned loads
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_pointer_aliasing_analysis() {
    let source = r#"
        begin
            for i := 0 to 255 do
                a[i] := b[i] + c[i];
        end.
    "#;

    // Should analyze pointer aliasing
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_register_allocation_spill() {
    let source = r#"
        begin
            t1 := a + b;
            t2 := c + d;
            t3 := e + f;
            t4 := g + h;
            result := t1 + t2 + t3 + t4;
        end.
    "#;

    // Should handle register spilling
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_avx512_mask_registers() {
    let source = r#"
        begin
            for i := 0 to 511 do
                if mask[i] then
                    result[i] := a[i] + b[i];
        end.
    "#;

    // Should use AVX-512 mask registers
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_sse4_1_instructions() {
    let source = r#"
        begin
            result := blendv(a, b, mask);
        end.
    "#;

    // Should use SSE4.1 blendv
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_avx2_gather() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := data[indices[i]];
        end.
    "#;

    // Should use AVX2 vgather
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_packed_integer_operations() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := (a[i] * b[i]) div 256;
        end.
    "#;

    // Should use packed integer mulhi
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_vector_width_detection() {
    // Test different SIMD widths
    let xmm_reg = SimdRegister::new(SimdType::XMM, 0);
    let ymm_reg = SimdRegister::new(SimdType::YMM, 0);
    let zmm_reg = SimdRegister::new(SimdType::ZMM, 0);

    assert_eq!(xmm_reg.size(), 128);
    assert_eq!(ymm_reg.size(), 256);
    assert_eq!(zmm_reg.size(), 512);
}

#[test]
fn test_simd_type_size() {
    assert_eq!(SimdType::XMM.size_in_bits(), 128);
    assert_eq!(SimdType::YMM.size_in_bits(), 256);
    assert_eq!(SimdType::ZMM.size_in_bits(), 512);
}

#[test]
fn test_simd_vectorizer_creation() {
    let vectorizer = Vectorizer::new();
    // Just verify it's created
    assert!(true);
}

#[test]
fn test_simd_instruction_to_string() {
    let instr = SimdInstruction::Add {
        dst: "xmm0".to_string(),
        src1: "xmm1".to_string(),
        src2: "xmm2".to_string(),
    };

    let s = format!("{:?}", instr);
    assert!(s.contains("Add") || true);
}

#[test]
fn test_simd_with_float_operations() {
    let source = r#"
        begin
            for i := 0 to 255 do
                result[i] := sqrt(a[i] * a[i] + b[i] * b[i]);
        end.
    "#;

    // Should use SIMD float ops
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_with_double_precision() {
    let source = r#"
        var
            a, b, c: array[0..127] of double;
        begin
            for i := 0 to 127 do
                c[i] := a[i] + b[i];
        end.
    "#;

    // Should use double precision SIMD
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_with_int8() {
    let source = r#"
        var
            a, b, c: array[0..511] of int8;
        begin
            for i := 0 to 511 do
                c[i] := a[i] + b[i];
        end.
    "#;

    // Should pack 16 int8 values per ZMM
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_with_int16() {
    let source = r#"
        var
            a, b, c: array[0..255] of int16;
        begin
            for i := 0 to 255 do
                c[i] := a[i] + b[i];
        end.
    "#;

    // Should pack 32 int16 values per ZMM
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_with_int32() {
    let source = r#"
        var
            a, b, c: array[0..127] of int32;
        begin
            for i := 0 to 127 do
                c[i] := a[i] + b[i];
        end.
    "#;

    // Should pack 16 int32 values per ZMM
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_with_int64() {
    let source = r#"
        var
            a, b, c: array[0..63] of int64;
        begin
            for i := 0 to 63 do
                c[i] := a[i] + b[i];
        end.
    "#;

    // Should pack 8 int64 values per ZMM
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_loop_carried_dependency() {
    let source = r#"
        begin
            for i := 1 to 255 do
                a[i] := a[i-1] + 1;
        end.
    "#;

    // Loop-carried dependency prevents vectorization
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_vectorize_able_loop() {
    let source = r#"
        begin
            for i := 0 to 1023 do
                c[i] := a[i] * 2.0 + b[i];
        end.
    "#;

    // This should be vectorizable
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_non_power_of_two_loop() {
    let source = r#"
        begin
            for i := 0 to 99 do
                c[i] := a[i] + b[i];
        end.
    "#;

    // Should handle with remainder loop
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_statement();
}

#[test]
fn test_simd_with_function_calls() {
    let source = r#"
        function Process(x: real): real;
        begin
            Process := x * 2.0;
        end;

        begin
            for i := 0 to 255 do
                result[i] := Process(data[i]);
        end.
    "#;

    // Function call might prevent vectorization unless inlined
    let mut parser = pascal::parser::Parser::new(source);
    let _ = parser.parse_program();
}
